using ICU
using Base.Test

# Tests for not overrunning buffer
str = "\u3b0"
upp = "\u3a5\u308\u301"

@test ICU.toupper(ICU.cvt_utf16(str^8)) == ICU.cvt_utf16(upp^8)

@testset "utext" begin
end

@testset "ustring (general string)" begin
end

@testset "ucasemap (Case mapping)" begin
    @test ICU.tolower("ABCDEF") == "abcdef"
    @test ICU.toupper("abcdef") == "ABCDEF"
    @test ICU.totitle("this") == "This"
end

@testset "ubrk (Word/Line/Sentence breaking)" begin
    str = ICU.cvt_utf16("This is a test of splitting sentences and words.  Scott is busy.\nHow does this\nsplit? Tell me.")
    for (sep, res) in ((UBRK.WORD,
                        [4,5,7,8,9,10,14,15,17,18,27,28,37,38,41,42,47,48,49,50,
                         55,56,58,59,63,64,65,68,69,73,74,78,79,84,85,86,90,91,93]),
                       (UBRK.SENTENCE, [50,65,79,86,93]),
                       (UBRK.LINE, [5,8,10,15,18,28,38,42,50,56,59,65,69,74,79,86,91,93]))
        brk = UBrk(sep, str)
        vec = Vector{Int}()
        while (v = ICU.next(brk)) != UBRK.DONE
            push!(vec, v)
        end
        close(brk)
        @test vec == res
    end
end

@testset "ucnv (Conversion)" begin
    @test ICU.tolower(ICU.cvt_utf16("ABCDEF")) == "abcdef"
    @test ICU.toupper(ICU.cvt_utf16("abcdef")) == "ABCDEF"
    @test ICU.totitle(ICU.cvt_utf16("this")) == "This"
end

@testset "ucol (Collation)" begin
end

@testset "ucal (Calendar)" begin
end

@testset "udat (Date)" begin
end

@testset "ucsdet (Character Set Detection)" begin
    csd = UCharsetDetector()
    for s in ("This is ASCII test, let's see how it fairs",
                    "にほんでは、近頃ちかごろ多おおくの人ひとが保育園ほいくえん問題もんだいについて話はなしている。特とくに東京とうきょうでは十分じゅうぶんな施設しせつがないので、子こどもを保育園ほいくえんに入いれることがとても大変たいへんだ。今いま私わたしは東京とうきょうに住すんでいるので、息子むすこを保育園ほいくえんに入いれるのは不可能ふかのうだろうと思おもっていた。しかし驚おどろいたことに、息子むすこは受うけ入いれてもらえた"))
        set!(csd, s)
        @test (m = detect(csd)) != nothing
        println(get_name(m))
    end
    close(csd)
end
