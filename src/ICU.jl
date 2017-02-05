"""
    ICU.jl - Wrapper for ICU (International Components for Unicode) library

    Some content of the documentation strings was derived from the ICU ubrk.h header file.
    (Those portions copyright (C) 1996-2015, International Business Machines Corporation
     and others)
"""
module ICU

import Base: parse, get, close

export set_locale!

include("../deps/deps.jl")
include("../deps/versions.jl")
include("compat5.jl")

@static if is_windows()
    # make sure versions match
    v1 = last(matchall(r"\d{2}", iculib))
    v2 = last(matchall(r"\d{2}", iculibi18n))
    v1 == v2 ||
        error("ICU library version mismatch $v1 != $v2 -- please correct $(realpath("../deps/deps.jl"))")
end

dliculib = Libdl.dlopen(iculib)

global version
global suffix

for (suf,ver) in [("",0);
                         [("_$i",i) for i in versions];
                         [("_$(string(i)[1])_$(string(i)[2])",i) for i in versions]]
    if Libdl.dlsym_e(dliculib, "u_strToUpper"*suf) != C_NULL
        @eval const version = $ver
        @eval const suffix  = $suf
        break
    end
end

_libicu(s, lib, p) = ( Symbol(string(p, s, suffix)), lib )

macro libdat(s)     ; _libicu(s, iculibi18n, "udat_")     ; end
macro libcol(s)     ; _libicu(s, iculibi18n, "ucol_")     ; end
macro libcal(s)     ; _libicu(s, iculibi18n, "ucal_")     ; end
macro libcnv(s)     ; _libicu(s, iculibi18n, "ucnv_")     ; end
macro libstr(s)     ; _libicu(s, iculib,     "u_str")     ; end
macro libbrk(s)     ; _libicu(s, iculib,     "ubrk_")     ; end
macro libtext(s)    ; _libicu(s, iculib,     "utext_")    ; end
macro libcasemap(s) ; _libicu(s, iculib,     "ucasemap_") ; end

typealias UBool Int8
typealias UChar UInt16
typealias UErrorCode Int32

FAILURE(x::Integer) = x > 0
SUCCESS(x::Integer) = x <= 0
U_BUFFER_OVERFLOW_ERROR = 15

const locale   = String[""]
const casemap  = Ptr{Void}[C_NULL]

## utext ##

export UText
type UText
    p::Ptr{Void}
    s

    function UText(str::ByteStr)
        err = UErrorCode[0]
        v = Vector{UInt8}(str)
        p = ccall(@libtext(openUTF8), Ptr{Void},
                  (Ptr{Void}, Ptr{UInt8}, Int64, Ptr{UErrorCode}),
                  C_NULL, v, sizeof(v), err)
        @assert SUCCESS(err[1])
        # Retain pointer to data so that it won't be GCed
        self = new(p, v)
        finalizer(self, close)
        self
    end

    function UText(str::UniStr)
        err = UErrorCode[0]
        v = Vector{UInt16}(str)
        p = ccall(@libtext(openUChars), Ptr{Void},
                  (Ptr{Void}, Ptr{UChar}, Int64, Ptr{UErrorCode}),
                  C_NULL, v, length(v)-1, err)
        @assert SUCCESS(err[1])
        # Retain pointer to data so that it won't be GCed
        self = new(p, v)
        finalizer(self, close)
        self
    end
end

close(t::UText) =
    t.p == C_NULL ||
        (ccall(@libtext(close), Void, (Ptr{Void},), t.p) ; t.p = C_NULL ; t.s = Void())

## ustring ##

_tolower(dest::Vector{UInt16}, destsiz, src, err) =
    ccall(@libstr(ToLower), Int32,
          (Ptr{UChar}, Int32, Ptr{UChar}, Int32, Ptr{UInt8}, Ptr{UErrorCode}),
          dest, destsiz, src, length(src)-1, locale[], err)

_toupper(dest::Vector{UInt16}, destsiz, src, err) =
    ccall(@libstr(ToUpper), Int32,
          (Ptr{UChar}, Int32, Ptr{UChar}, Int32, Ptr{UInt8}, Ptr{UErrorCode}),
          dest, destsiz, src, length(src)-1, locale[], err)

_foldcase(dest::Vector{UInt16}, destsiz, src, err) =
    ccall(@libstr(FoldCase), Int32,
          (Ptr{UChar}, Int32, Ptr{UChar}, Int32, UInt32, Ptr{UErrorCode}),
          dest, destsiz, src, length(src)-1, 0, err)

_totitle(dest::Vector{UInt16}, destsiz, src, breakiter, err) =
    ccall(@libstr(ToTitle), Int32,
          (Ptr{UChar}, Int32, Ptr{UChar}, Int32, Ptr{Void}, Ptr{UInt8}, Ptr{UErrorCode}),
          dest, destsiz, src, length(src)-1, breakiter, locale[], err)

for f in (:tolower, :toupper, :foldcase)
    uf = Symbol(string('_',f))
    @eval begin
        function ($f)(s::UniStr)
            src = Vector{UInt16}(s)
            destsiz = Int32(length(src))
            dest = zeros(UInt16, destsiz)
            err = UErrorCode[0]
            n = ($uf)(dest, destsiz, src, err)
            # Retry with large enough buffer if got buffer overflow
            if err[1] == U_BUFFER_OVERFLOW_ERROR
                err[1] = 0
                destsiz = n + 1
                dest = zeros(UInt16, destsiz)
                n = ($uf)(dest, destsiz, src, err)
            end
            FAILURE(err[1]) && error("failed to map case")
            return UniStr(dest[1:n+1])
        end
    end
end

function totitle(s::UniStr)
    src = Vector{UInt16}(s)
    destsiz = Int32(length(src))
    dest = zeros(UInt16, destsiz)
    err = UErrorCode[0]
    breakiter = ccall(@libcasemap(getBreakIterator), Ptr{Void}, (Ptr{Void},), casemap[])
    n = _totitle(dest, destsiz, src, breakiter, err)
    # Retry with large enough buffer if got buffer overflow
    if err[1] == U_BUFFER_OVERFLOW_ERROR
        err[1] = 0
        destsiz = n + 1
        dest = zeros(UInt16, destsiz)
        n = _totitle(dest, destsiz, src, breakiter, err)
    end
    FAILURE(err[1]) && error("failed to map case")
    return UniStr(dest[1:n+1])
end

## ucasemap ##

for f in (:ToLower, :ToUpper, :FoldCase, :ToTitle)
    lf = Symbol(lowercase(string(f)))
    ff = Symbol(string("utf8", f))
    uf = Symbol(string('_',lf))
    @eval begin
        ($uf)(dest::Vector{UInt8}, destsiz, src, err) =
            ccall(@libcasemap($ff), Int32,
                  (Ptr{Void}, Ptr{UInt8}, Int32, Ptr{UInt8}, Int32, Ptr{UErrorCode}),
                  casemap[], dest, destsiz, src, sizeof(src), err)
        function ($lf)(s::ByteStr)
            src = Vector{UInt8}(s)
            destsiz = Int32(sizeof(src))
            dest = zeros(UInt8, destsiz)
            err = UErrorCode[0]
            n = ($uf)(dest, destsiz, Vector{UInt8}(src), err)
            # Retry with large enough buffer if got buffer overflow
            if err[1] == U_BUFFER_OVERFLOW_ERROR
                err[1] = 0
                destsiz = n
                dest = zeros(UInt8, destsiz)
                n = ($uf)(dest, destsiz, src, err)
            end
            FAILURE(err[1]) && error("failed to map case")
            return cvt_utf8(dest[1:n])
        end
    end
end

## ubrk ##

typealias UBrkType Int32

export UBRK
module UBRK
const CHARACTER = Int32(0)
const WORD      = Int32(1)
const LINE      = Int32(2)
const SENTENCE  = Int32(3)
const TITLE     = Int32(4) # Deprecated API (use word break iterator)
const COUNT     = Int32(5)

"""Value indicating all text boundaries have been returned."""
const DONE      = Int32(-1)

"""
    Constants for the word break tags returned by ICU.get_rule_status().
    A range of values is defined for each category of word, to allow for
    further subdivisions of a category in future releases.
    Applications should check for tag values falling within the range,
    rather than for single individual values.
"""
const WORD_TAG = 0

"""
    Tag value for "words" that do not fit into any of other categories.
    Includes spaces and most punctuation.
"""
const WORD_NONE  = Int32(0)
"""
    Upper bound for tags for uncategorized words.
"""
const WORD_NONE_LIMIT = Int32(100)
"""
    Tag value for words that appear to be numbers, lower limit.
"""
const WORD_NUMBER     = Int32(100)
"""
    Tag value for words that appear to be numbers, upper limit.
"""
const WORD_NUMBER_LIMIT   = Int32(200)
"""
    Tag value for words that contain letters, excluding hiragana, katakana or
    ideographic characters, lower limit.
"""
const WORD_LETTER         = Int32(200)
"""
    Tag value for words containing letters, upper limit
"""
const WORD_LETTER_LIMIT   = Int32(300)
"""
    Tag value for words containing kana characters, lower limit
"""
const WORD_KANA           = Int32(300)
"""
    Tag value for words containing kana characters, upper limit
"""
const WORD_KANA_LIMIT     = Int32(400)
"""
    Tag value for words containing ideographic characters, lower limit
"""
const WORD_IDEO           = Int32(400)
"""
    Tag value for words containing ideographic characters, upper limit
"""
const WORD_IDEO_LIMIT     = Int32(500)

"""
    Constants for the line break tags returned by ICU.get_rule_status().
    A range of values is defined for each category of word, to allow for
    further subdivisions of a category in future releases.
    Applications should check for tag values falling within the range,
    rather than for single individual values.
"""
const LINE_TAG = 0

"""
    Tag value for soft line breaks, positions at which a line break
    is acceptable but not required
"""
const LINE_SOFT            = Int32(0)
"""
    Upper bound for soft line breaks.
"""
const LINE_SOFT_LIMIT      = Int32(100)
"""
    Tag value for a hard, or mandatory line break
"""
const LINE_HARD            = Int32(100)
"""
    Upper bound for hard line breaks.
"""
const LINE_HARD_LIMIT      = Int32(200)

"""
    Constants for the sentence break tags returned by ICU.get_rule_status().
    A range of values is defined for each category of sentence, to allow for
    further subdivisions of a category in future releases.
    Applications should check for tag values falling within the range,
    rather than for single individual values.
"""
const SENTENCE_TAG = 0

"""
    Tag value for for sentences  ending with a sentence terminator
    ('.', '?', '!', etc.) character, possibly followed by a
    hard separator (CR, LF, PS, etc.)
"""
const SENTENCE_TERM       = Int32(0)
"""
    Upper bound for tags for sentences ended by sentence terminators.
"""
const SENTENCE_TERM_LIMIT = Int32(100)
"""
    Tag value for for sentences that do not contain an ending
    sentence terminator ('.', '?', '!', etc.) character, but
    are ended only by a hard separator (CR, LF, PS, etc.) or end of input.
"""
const SENTENCE_SEP        = Int32(100)
"""
    Upper bound for tags for sentences ended by a separator.
"""
const SENTENCE_SEP_LIMIT  = Int32(200)
end # module UBRK

export UBrk

"""
    UBrk is a type along with methods for finding the location of boundaries in text.
    A UBrk maintains a current position and scan over text returning the index of characters
    where boundaries occur.

    Line boundary analysis determines where a text string can be broken when line-wrapping.
    The mechanism correctly handles punctuation and hyphenated words.

    Note: The locale keyword "lb" can be used to modify line break behavior according to
    the CSS level 3 line-break options, see <http://dev.w3.org/csswg/css-text/#line-breaking>.
    For example: "ja@lb=strict", "zh@lb=loose".

    Sentence boundary analysis allows selection with correct interpretation of periods within
    numbers and abbreviations, and trailing punctuation marks such as quotation marks and
    parentheses.

    Note: The locale keyword "ss" can be used to enable use of segmentation suppression data
    (preventing breaks in English after abbreviations such as "Mr." or "Est.", for example),
    as follows: "en@ss=standard".

    Word boundary analysis is used by search and replace functions, as well as within text
    editing applications that allow the user to select words with a double click.
    Word selection provides correct interpretation of punctuation marks within and following
    words. Characters that are not part of a word, such as symbols or punctuation marks,
    have word-breaks on both sides.

    Character boundary analysis identifies the boundaries of "Extended Grapheme Clusters",
    which are groupings of codepoints that should be treated as character-like units for
    many text operations.

    Please see Unicode Standard Annex #29, Unicode Text Segmentation,
    http://www.unicode.org/reports/tr29/ for additional information
    on grapheme clusters and guidelines on their use.

    Title boundary analysis locates all positions, typically starts of words, that should
    be set to Title Case when title casing the text.

    The text boundary positions are found according to the rules described in
    Unicode Standard Annex #29, Text Boundaries, and Unicode Standard Annex #14,
    Line Breaking Properties.  These are available at http://www.unicode.org/reports/tr14/
    and http://www.unicode.org/reports/tr29/.
"""
type UBrk
    p::Ptr{Void}
    s

    function UBrk(kind::Integer, s::Vector{UInt16}, loc::ASCIIStr)
        err = UErrorCode[0]
        p = ccall(@libbrk(open), Ptr{Void},
                  (UBrkType, Cstring, Ptr{UChar}, Int32, Ptr{UErrorCode}),
                  kind, loc, s, length(s)-1, err)
        @assert SUCCESS(err[1])
        # Retain pointer to input vector, otherwise it might be GCed
        self = new(p, s)
        finalizer(self, close)
        self
    end
    function UBrk(kind::Integer, s::Vector{UInt16})
        err = UErrorCode[0]
        p = ccall(@libbrk(open), Ptr{Void},
                  (UBrkType, Ptr{UInt8}, Ptr{UChar}, Int32, Ptr{UErrorCode}),
                  kind, C_NULL, s, length(s)-1, err)
        @assert SUCCESS(err[1])
        # Retain pointer to input vector, otherwise it might be GCed
        self = new(p, s)
        finalizer(self, close)
        self
    end
end
UBrk(kind::Integer, s::UniStr) = UBrk(kind, Vector{UInt16}(s)[1:end-1])
UBrk(kind::Integer, s::UniStr, loc::AbstractString) = UBrk(kind, Vector{UInt16}(s)[1:end-1], ascii(loc))

"""
    Close the Break Iterator and return any resource, if not already closed
"""
close(bi::UBrk) =
    bi.p == C_NULL ||
        (ccall(@libbrk(close), Void, (Ptr{Void},), bi.p) ; bi.p = C_NULL ; bi.s = Void())

"""
    Determine the most recently-returned text boundary.

    Arguments:
    bi - The break iterator to use.
    Returns the character index most recently returned by next, previous, first, or last.
"""
current(bi::UBrk) = ccall(@libbrk(current), Int32, (Ptr{Void},), bi.p)

"""
    Advance the iterator to the boundary following the current boundary.
    Returns the character index of the next text boundary, or UBRK.DONE
    if all text boundaries have been returned.
"""
next(bi::UBrk) = ccall(@libbrk(next), Int32, (Ptr{Void},), bi.p)

"""
    Set the iterator position to the boundary preceding the current boundary.
    Returns the character index of the preceding text boundary, or UBRK.DONE
    if all text boundaries have been returned.
"""
previous(bi::UBrk) = ccall(@libbrk(previous), Int32, (Ptr{Void},), bi.p)

"""
    Set the iterator position to zero, the start of the text being scanned.
    Returns the new iterator position (zero).
"""
first(bi::UBrk) = ccall(@libbrk(first), Int32, (Ptr{Void},), bi.p)

"""
    Set the iterator position to the index immediately <EM>beyond</EM> the last character in
    the text being scanned.
    This is not the same as the last character.
    Returns the character offset immediately <EM>beyond</EM> the last character in the
    text being scanned.
"""
last(bi::UBrk) = ccall(@libbrk(last), Int32, (Ptr{Void},), bi.p)

"""
    Set the iterator position to the first boundary preceding the specified offset.
    The new position is always smaller than offset, or UBRK.DONE

    Arguments:
    bi - The break iterator to use
    offset - The offset to begin scanning.
    Returns the text boundary preceding offset, or UBRK.DONE
"""
preceding(bi::UBrk, off) = ccall(@libbrk(preceding), Int32, (Ptr{Void}, Int32), bi.p, off)

"""
    Advance the iterator to the first boundary following the specified offset.
    The value returned is always greater than offset, or UBRK.DONE

    Arguments:
    - bi - The break iterator to use.
    - offset - The offset to begin scanning.
    Returns the text boundary following offset, or UBRK.DONE
"""
following(bi::UBrk, off) = ccall(@libbrk(following), Int32, (Ptr{Void}, Int32), bi.p, off)

"""
    Get a locale for which text breaking information is available.
    A UBrk in a locale returned by this function will perform the correct
    text breaking for the locale.

    Arguments:
    index - The index of the desired locale.

    Returns: A locale for which number text breaking information is available, or 0 if none.
"""
function get_available(index)
    loc = ccall(@libbrk(getAvailable), Ptr{UInt8}, (Int32, ), index)
    loc == C_NULL ? "" : ASCIIStr(loc)
end

"""
    Determine how many locales have text breaking information available.
    This function is most useful as determining the loop ending condition for
    calls to get_available.

    Returns: the number of locales for which text breaking information is available.
"""
count_available() = ccall(@libbrk(countAvailable), Int32, ())

"""
    Returns true if the specfied position is a boundary position.  As a side
    effect, leaves the iterator pointing to the first boundary position at
    or after "offset".

    Arguments:
    - bi - The break iterator to use.
    - offset - The offset to check.
    Returns true if "offset" is a boundary position.
"""
isboundary(bi::UBrk, off) = ccall(@libbrk(isBoundary), Int32, (Ptr{Void}, Int32), bi.p, off) != 0

"""
    Return the status from the break rule that determined the most recently returned break
    position.  The values appear in the rule source within brackets, {123}, for example.
    For rules that do not specify a status, a default value of 0 is returned.
"""
get_rule_status(bi::UBrk) =
    ccall(@libbrk(getRuleStatus), Int32, (Ptr{Void},), bi.p)

"""
    Get the statuses from the break rules that determined the most recently
    returned break position.  The values appear in the rule source within brackets,
    {123}, for example.  The default status value for rules that do not explicitly
    provide one is zero.

    For word break iterators, the possible values are defined module UBRK

    Arguments:
    bi - The break iterator to use
    fillinvec - an array to be filled in with the status values.
    capacity - the length of the supplied vector.  A length of zero causes
               the function to return the number of status values, in the
               normal way, without attemtping to store any values.
    status - receives error codes.

    Returns: The number of rule status values from rules that determined
             the most recent boundary returned by the break iterator.
"""
function get_rule_status_vec(bi::UBrk, fillinvec::Vector{Int32})
    # ccall(@libbrk(getRuleStatusVec), Int32,
    #       (UBreakIterator *bi, int32_t *fillInVec, int32_t capacity, UErrorCode *status);
end

"""
    Return the locale of the break iterator. You can choose between
    the valid and the actual locale.

    Arguments:
    bi - break iterator
    type - locale type (valid or actual)
    status - error code

    Return: locale string
"""
function get_locale_by_type(bi::UBrk, typ, status)
    # ccall(@libbrk(getLocaleByType), Ptr{UInt8},
    #       (const UBreakIterator *bi, ULocDataLocaleType type, UErrorCode* status);
end

"""
    Sets an existing iterator to point to a new piece of text

    Arguments:
    bi - The iterator to use
    text - The text to be set

    Returns status code
"""
function set! end

set!(bi::UBrk, str::AbstractString) = set!(bi, cvt_utf16(str))
set!(bi::UBrk, str::UniStr) = set!(bi, Vector{UInt16}(str))

function set!(bi::UBrk, v::Vector{UInt16})
    err = UErrorCode[0]
    ccall(@libbrk(setText), Void,
          (Ptr{Void}, Ptr{UChar}, Int32, Ptr{UErrorCode}),
          bi.p, v, length(v)-1, err)
    # Retain pointer so that it won't be GCed
    bi.s = v
    @assert SUCCESS(err[1])
    err[1]
end

"""
    Sets an existing iterator to point to a new piece of text.

    All index positions returned by break iterator functions are
    native indices from the UText. For example, when breaking UTF-8
    encoded text, the break positions returned by next, previous, etc.
    will be UTF-8 string indices, not UTF-16 positions.

    Arguments:
    bi - The iterator to use
    text - The text to be set.
           This function makes a shallow clone of the supplied UText.  This means
           that the caller is free to immediately close or otherwise reuse the
           UText that was passed as a parameter, but that the underlying text itself
           must not be altered while being referenced by the break iterator.

    Returns status code
"""
function set!(bi::UBrk, t::UText)
    err = UErrorCode[0]
    ccall(@libbrk(setUText), Void,
          (Ptr{Void}, Ptr{Void}, Ptr{UErrorCode}),
          bi.p, t.p, err)
    @assert SUCCESS(err[1])
    err[1]
end

## ucnv ##

export UConverter
type UConverter
    p::Ptr{Void}

    function UConverter(name::ASCIIStr)
        err = UErrorCode[0]
        p = ccall(@libcnv(open), Ptr{Void}, (Cstring, Ptr{UErrorCode}), name, err)
        SUCCESS(err[1]) || error("ICU: could not open converter ", name)
        self = new(p)
        finalizer(self, close)
        self
    end
end

close(c::UConverter) =
    c.p == C_NULL || (ccall(@libcnv(close), Void, (Ptr{Void},), c.p); c.p = C_NULL)

type UConverterPivot
    buf::Vector{UChar}
    pos::Vector{Ptr{UChar}}

    function UConverterPivot(n::Int)
        buf = Array(UChar, n)
        p = pointer(buf)
        new(buf, [p,p])
    end
end

function convert!(dstcnv::UConverter, srccnv::UConverter,
                  dst::IOBuffer, src::IOBuffer, pivot::UConverterPivot,
                  reset::Bool=false, flush::Bool=true)
    p = Ptr{UInt8}[pointer(dst.data, position(dst)+1),
                   pointer(src.data, position(src)+1)]
    p0 = copy(p)
    err = UErrorCode[0]
    ccall(@libcnv(convertEx), Void,
          (Ptr{Void}, Ptr{Void},
           Ptr{Ptr{UInt8}}, Ptr{UInt8}, Ptr{Ptr{UInt8}}, Ptr{UInt8},
           Ptr{UChar}, Ptr{Ptr{UChar}}, Ptr{Ptr{UChar}}, Ptr{UChar},
           UBool, UBool, Ptr{UErrorCode}),
          dstcnv.p, srccnv.p,
          pointer(p, 1), pointer(dst.data, length(dst.data)+1),
          pointer(p, 2), pointer(src.data, src.size+1),
          pointer(pivot.buf, 1),
          pointer(pivot.pos, 1),
          pointer(pivot.pos, 2),
          pointer(pivot.buf, length(pivot.buf)+1),
          reset, flush, err)
    dst.size += p[1] - p0[1]
    dst.ptr += p[1] - p0[1]
    src.ptr += p[2] - p0[2]
    err[1] == U_BUFFER_OVERFLOW_ERROR && return true
    @assert SUCCESS(err[1])
    false
end

function to_uchars(cnv::UConverter, b::Vector{UInt8})
    u = zeros(UInt16, 2*length(b)+1)
    err = UErrorCode[0]
    n = ccall(@libcnv(toUChars), Int32,
              (Ptr{Void}, Ptr{UChar}, Int32, Ptr{UInt8}, Int32, Ptr{UErrorCode}),
              cnv.p, u, length(u), b, length(b), err)
    SUCCESS(err[1]) || error("ICU: could not convert string")
    UniStr(u[1:n+1])
end

"""
    Determines if the converter contains ambiguous mappings of the same character or not.

    Arguments:
    cnv - the converter to be tested

    Return `true` if the converter contains ambiguous mapping of the same
    character, `false` otherwise.
"""
function isambiguous(cnv::UConverter)
    err = UErrorCode[0]
    v = ccall(@libcnv(isAmbiguous), Bool, (Ptr{Void}, Ptr{UErrorCode}), cnv.p, err)
    SUCCESS(err[1]) || error("ICU: internal error in ucnv_isFixedWidth")
    v
end

"""
    Detects Unicode signature byte sequences at the start of the byte stream
    and returns the charset name of the indicated Unicode charset.
    NULL is returned when no Unicode signature is recognized.
    The number of bytes in the signature is output as well.

    The caller can ucnv_open() a converter using the charset name.
    The first code unit (UChar) from the start of the stream will be U+FEFF
    (the Unicode BOM/signature character) and can usually be ignored.

    For most Unicode charsets it is also possible to ignore the indicated
    number of initial stream bytes and start converting after them.
    However, there are stateful Unicode charsets (UTF-7 and BOCU-1) for which
    this will not work. Therefore, it is best to ignore the first output UChar
    instead of the input signature bytes.

    Arguments:
    source    - The source string in which the signature should be detected.

    Returns the name of the encoding detected, and the length of the signature
"""
function detect_unicode_signature(src::Vector{UInt8})
    err = UErrorCode[0]
    sig = Int32[0]
    p = ccall(@libcnv(detectUnicodeSignature), Ptr{UInt8},
              (Ptr{UInt8}, Int32, Ptr{Int32}, Ptr{UErrorCode}),
              src, sizeof(src), sig, err)
    SUCCESS(err[1]) || error("ICU: internal error in ucnv_detectUnicodeSignature")
    return (p == C_NULL ? UTF8Str() : UTF8Str(p), sig[1])
end

"""
    Returns the number of chars held in the converter's internal state
    because more input is needed for completing the conversion. This function is
    useful for mapping semantics of ICU's converter interface to those of iconv,
    and this information is not needed for normal conversion.

    Arguments:
    cnv - The converter in which the input is held as internal state

    Returns the number of chars in the state or -1 if an error is encountered.
"""
function from_ucount_pending(cnv::UConverter)
    err = UErrorCode[0]
    v = ccall(@libcnv(toUCountPending), Int32, (Ptr{Void}, Ptr{UErrorCode}), cnv.p, err)
    SUCCESS(err[1]) || error("ICU: internal error in ucnv_fromUCountPending")
    v
end

"""
    Returns the number of chars held in the converter's internal state
    because more input is needed for completing the conversion. This function is
    useful for mapping semantics of ICU's converter interface to those of iconv,
    and this information is not needed for normal conversion.

    Arguments:
    cnv - The converter in which the input is held as internal state

    Returns the number of chars in the state or -1 if an error is encountered.
"""
function to_ucount_pending(cnv::UConverter)
    err = UErrorCode[0]
    v = ccall(@libcnv(toUCountPending), Int32, (Ptr{Void}, Ptr{UErrorCode}), cnv.p, err)
    SUCCESS(err[1]) || error("ICU: internal error in ucnv_toUCountPending")
    v
end

"""
    Returns whether or not the charset of the converter has a fixed number of bytes
    per charset character.
    An example of this are converters that are of the type UCNV_SBCS or UCNV_DBCS.
    Another example is UTF-32 which is always 4 bytes per character.
    A Unicode code point may be represented by more than one UTF-8 or UTF-16 code unit
    but a UTF-32 converter encodes each code point with 4 bytes.
    Note: This method is not intended to be used to determine whether the charset has a
    fixed ratio of bytes to Unicode codes <i>units</i> for any particular Unicode encoding form.
    `false` is returned with the UErrorCode if error occurs or cnv is NULL.

    cnv       The converter to be tested
    status    ICU error code in/out paramter
    Returns `true` if the converter is fixed-width
"""
function is_fixed_width(cnv::UConverter)
    err = UErrorCode[0]
    v = ccall(@libcnv(isFixedWidth), Bool, (Ptr{Void}, Ptr{UErrorCode}), cnv.p, err)
    SUCCESS(err[1]) || error("ICU: internal error in ucnv_isFixedWidth")
    v
end

## ucol ##

export UCollator
type UCollator
    p::Ptr{Void}

    UCollator() = new(C_NULL)
    function UCollator(loc)
        p = _ucol_open(loc)
        self = new(p)
        finalizer(self, close)
        self
    end
end

function _ucol_open(loc::ASCIIStr)
    err = UErrorCode[0]
    p = (loc == ""
         ? ccall(@libcol(open), Ptr{Void}, (Ptr{UInt8}, Ptr{UErrorCode}), C_NULL, err)
         : ccall(@libcol(open), Ptr{Void}, (Cstring, Ptr{UErrorCode}), loc, err))
    SUCCESS(err[1]) || error("ICU: could not open collator for locale ", loc)
    p
end
_ucol_open(loc::AbstractString) = _ucol_open(ASCIIStr(loc))

close(c::UCollator) =
    c.p == C_NULL || (ccall(@libcol(close), Void, (Ptr{Void},), c.p) ; c.p = C_NULL)

function strcoll end

strcoll(c::UCollator, a::ByteStr, b::ByteStr) = strcoll(c, Vector{UInt8}(a), Vector{UInt8}(b))
function strcoll(c::UCollator, a::Vector{UInt8}, b::Vector{UInt8})
    err = UErrorCode[0]
    o = ccall(@libcol(strcollUTF8), Int32,
              (Ptr{Void}, Ptr{UInt8}, Int32, Ptr{UInt8}, Int32, Ptr{UErrorCode}),
              c.p, a, sizeof(a), b, sizeof(b), err)
    @assert SUCCESS(err[1])
    o
end

strcoll(c::UCollator, a::UniStr, b::UniStr) = strcoll(c, Vector{UInt16}(a), Vector{UInt16}(b))
function strcoll(c::UCollator, a::Vector{UInt16}, b::Vector{UInt16})
    err = UErrorCode[0]
    o = ccall(@libcol(strcoll), Int32,
              (Ptr{Void}, Ptr{UChar}, Int32, Ptr{UChar}, Int32, Ptr{UErrorCode}),
              c.p, a, length(a)-1, b, length(b)-1, err)
    @assert SUCCESS(err[1])
    o
end
strcoll(c::UCollator, a::ByteStr, b::UniStr) = strcoll(c, cvt_utf16(a), b)
strcoll(c::UCollator, a::UniStr, b::ByteStr) = strcoll(c, a, cvt_utf16(b))

## calendar ##

export UCAL

module UCAL
for (i,a) in enumerate([
        :ERA,
        :YEAR,
        :MONTH,
        :WEEK_OF_YEAR,
        :WEEK_OF_MONTH,
        :DATE,
        :DAY_OF_YEAR,
        :DAY_OF_WEEK,
        :DAY_OF_WEEK_IN_MONTH,
        :AM_PM,
        :HOUR,
        :HOUR_OF_DAY,
        :MINUTE,
        :SECOND,
        :MILLISECOND,
        :ZONE_OFFSET,
        :DST_OFFSET,
        :YEAR_WOY,
        :DOW_LOCAL,
        :EXTENDED_YEAR,
        :JULIAN_DAY,
        :MILLISECONDS_IN_DAY,
        :IS_LEAP_MONTH
    ])
    @eval const $a = Int32($i - 1)
end
end # module UCAL

typealias UDate Float64

export UCalendar
type UCalendar
    ptr::Ptr{Void}

    UCalendar(tz::UniStr) = UCalendar(Vector{UInt16}(tz))
    function UCalendar(tz::Vector{UInt16})
        err = UErrorCode[0]
        p = ccall(@libcal(open), Ptr{Void},
                  (Ptr{UChar}, Int32, Ptr{UInt8}, Int32, Ptr{UErrorCode}),
                  tz, length(tz)-1, locale[], 0, err)
        self = new(p)
        finalizer(self, close)
        self
    end
    function UCalendar()
        err = UErrorCode[0]
        p = ccall(@libcal(open), Ptr{Void},
                  (Ptr{UChar}, Int32, Ptr{UInt8}, Int32, Ptr{UErrorCode}),
                  C_NULL, 0, locale[], 0, err)
        self = new(p)
        finalizer(self, close)
        self
    end
end

UCalendar(timezone::AbstractString) = UCalendar(cvt_utf16(timezone))

close(c::UCalendar) =
    c.ptr == C_NULL || (ccall(@libcal(close), Void, (Ptr{Void},), c.ptr) ; c.ptr = C_NULL)

getnow() = ccall(@libcal(getNow), UDate, ())

function get_millis(cal::UCalendar)
    err = UErrorCode[0]
    ccall(@libcal(getMillis), UDate, (Ptr{Void}, Ptr{UErrorCode}), cal.ptr, err)
end

function set_millis!(cal::UCalendar, millis::UDate)
    err = UErrorCode[0]
    ccall(@libcal(setMillis), Void,
          (Ptr{Void}, UDate, Ptr{UErrorCode}),
          cal.ptr, millis, err)
end

function set_date!(cal::UCalendar, y::Integer, m::Integer, d::Integer)
    err = UErrorCode[0]
    ccall(@libcal(setDate), Void,
          (Ptr{Void}, Int32, Int32, Int32, Ptr{UErrorCode}),
          cal.ptr, y, m-1, d, err)
end

function set_datetime!(cal::UCalendar, y::Integer, mo::Integer, d::Integer,
                     h::Integer, mi::Integer, s::Integer)
    err = UErrorCode[0]
    ccall(@libcal(setDateTime), Void,
          (Ptr{Void}, Int32, Int32, Int32, Int32, Int32, Int32, Ptr{UErrorCode}),
          cal.ptr, y, mo-1, d, h, mi, s, err)
end

function clear!(cal::UCalendar)
    err = UErrorCode[0]
    ccall(@libcal(clear), Void,
          (Ptr{Void}, Ptr{UErrorCode}),
          cal.ptr, err)
end

function get(cal::UCalendar, field::Int32)
    err = UErrorCode[0]
    ccall(@libcal(get), Int32,
          (Ptr{Void},Int32,Ptr{UErrorCode}),
          cal.ptr, field, err)
end
get(cal::UCalendar, fields::Vector{Int32}) = [get(cal,f) for f in fields]

function add!(cal::UCalendar, field::Int32, amount::Integer)
    err = UErrorCode[0]
    ccall(@libcal(add), Int32,
          (Ptr{Void},Int32,Int32,Ptr{UErrorCode}),
          cal.ptr, field, amount, err)
end

set!(cal::UCalendar, field::Int32, val::Integer) =
    ccall(@libcal(set), Void, (Ptr{Void}, Int32, Int32), cal.ptr, field, val)

function get_timezone_displayname(cal::UCalendar)
    bufsz = 64
    buf = zeros(UInt16, bufsz)
    err = UErrorCode[0]
    len = ccall(@libcal(getTimeZoneDisplayName), Int32,
                (Ptr{Void}, Int32, Ptr{UInt8}, Ptr{UChar}, Int32, Ptr{UErrorCode}),
                cal.ptr, 1, locale[], buf, bufsz, err)
    UniStr(buf[1:len+1])
end

function get_default_timezone()
    bufsz = 64
    buf = zeros(UInt16, bufsz)
    err = UErrorCode[0]
    len = ccall(@libcal(getDefaultTimeZone), Int32,
                (Ptr{UChar}, Int32, Ptr{UErrorCode}),
                buf, bufsz, err)
    UniStr(buf[1:len+1])
end

export UDAT
module UDAT
const NONE     = Int32(-1)
const FULL     = Int32(0)
const LONG     = Int32(1)
const MEDIUM   = Int32(2)
const SHORT    = Int32(3)
const RELATIVE = Int32(128)
end # module UDAT

export UDateFormat
type UDateFormat
    ptr::Ptr{Void}

    UDateFormat(tstyle::Integer, dstyle::Integer, tz::UniStr) =
        UDateFormat(tstyle, dstyle, Vector{UInt16}(tz))
    function UDateFormat(tstyle::Integer, dstyle::Integer, tz::Vector{UInt16})
        err = UErrorCode[0]
        p = ccall(@libdat(open), Ptr{Void},
                  (Int32, Int32, Ptr{UInt8}, Ptr{UChar}, Int32,
                   Ptr{UChar}, Int32, Ptr{UErrorCode}),
                  tstyle, dstyle, locale[], tz, length(tz)-1, C_NULL, 0, err)
        FAILURE(err[1]) && error("bad date format")
        self = new(p)
        finalizer(self, close)
        self
    end

    UDateFormat(pattern::UniStr, tz::UniStr) =
        UDateFormat(Vector{UInt16}(pattern), Vector{UInt16}(tz))
    function UDateFormat(pattern::Vector{UInt16}, tz::Vector{UInt16})
        err = UErrorCode[0]
        p = ccall(@libdat(open), Ptr{Void},
                  (Int32, Int32, Ptr{UInt8}, Ptr{UChar}, Int32,
                   Ptr{UChar}, Int32, Ptr{UErrorCode}),
                  -2, -2, locale[], tz, length(tz)-1,
                  pattern, length(pattern)-1, err)
        FAILURE(err[1]) && error("bad date format")
        self = new(p)
        finalizer(self, close)
        self
    end
end

UDateFormat(pattern::AbstractString, tz::AbstractString) =
    UDateFormat(cvt_utf16(pattern), cvt_utf16(tz))

UDateFormat(tstyle::Integer, dstyle::Integer, tz::AbstractString) =
    UDateFormat(tstyle, dstyle, cvt_utf16(tz))

close(df::UDateFormat) =
    df.ptr == C_NULL || (ccall(@libdat(close), Void, (Ptr{Void},), df.ptr) ; df.ptr = C_NULL)

function format(df::UDateFormat, millis::Float64)
    err = UErrorCode[0]
    buflen = 64
    buf = zeros(UChar, buflen)
    len = ccall(@libdat(format), Int32,
                (Ptr{Void}, Float64, Ptr{UChar}, Int32, Ptr{Void}, Ptr{UErrorCode}),
                df.ptr, millis, buf, buflen, C_NULL, err)
    FAILURE(err[1]) && error("failed to format time")
    UniStr(buf[1:len+1])
end

parse(df::UDateFormat, s::AbstractString) = parse(df, cvt_utf16(s))
parse(df::UDateFormat, s::UniStr) = parse(df, Vector{UInt16}(s))
function parse(df::UDateFormat, s16::Vector{UInt16})
    err = UErrorCode[0]
    ret = ccall(@libdat(parse), Float64,
                (Ptr{Void}, Ptr{UChar}, Int32, Ptr{Int32}, Ptr{UErrorCode}),
                df.ptr, s16, length(s16)-1, C_NULL, err)
    FAILURE(err[1]) && error("failed to parse string")
    ret
end

# end # module Udat

function test_ucalendar()
    cal = UCalendar("America/Los_Angeles")
    setMillis(cal, getNow())
    fields = [UCAL.YEAR, UCAL.MONTH, UCAL.DATE,
              UCAL.HOUR_OF_DAY, UCAL.MINUTE, UCAL.SECOND]
    println(get(cal,fields))
    clear(cal)
    df = UDateFormat()
    s = format(df, getNow())
    show(s)
end

const collator = UCollator()

function set_locale!(loc::ASCIIStr)
    if casemap[] != C_NULL
        ccall(@libcasemap(close), Void, (Ptr{Void},), casemap[])
        casemap[] = C_NULL
    end
    close(collator)
    collator.p = _ucol_open(loc)
    finalizer(collator, close)
    err = UErrorCode[0]
    casemap[] = (loc == ""
                 ? ccall(@libcasemap(open), Ptr{Void}, (Ptr{UInt8}, Int32, Ptr{UErrorCode}),
                         C_NULL, 0, err)
                 : ccall(@libcasemap(open), Ptr{Void}, (Cstring, Int32, Ptr{UErrorCode}),
                         loc, 0, err))
    FAILURE(err[1]) && error("could not set casemap")
    locale[] = loc
end

set_locale!(loc::AbstractString) = set_locale!(ASCIIStr(loc))

## init ##

set_locale!("")

end # module ICU
