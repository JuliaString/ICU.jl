using ICU
using Base.Test

# Tests for not overrunning buffer
str = "\u3b0"
upp = "\u3a5\u308\u301"

@test ICU.toupper(ICU.cvt_utf16(str^8)) == ICU.cvt_utf16(upp^8)

@testset "utext" begin
end

@testset "ustring" begin
end

@testset "ucasemap" begin
    @test ICU.tolower("ABCDEF") == "abcdef"
    @test ICU.toupper("abcdef") == "ABCDEF"
    @test ICU.totitle("this") == "This"
end

@testset "ubrk" begin
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

@testset "ucnv" begin
    @test ICU.tolower(ICU.cvt_utf16("ABCDEF")) == "abcdef"
    @test ICU.toupper(ICU.cvt_utf16("abcdef")) == "ABCDEF"
    @test ICU.totitle(ICU.cvt_utf16("this")) == "This"
end

@testset "ucol" begin
end

@testset "ucal" begin
end

@testset "udat" begin
end
