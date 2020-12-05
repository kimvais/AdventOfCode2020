module AoC2020.Tests

open Xunit
open Xunit.Abstractions
open AoC2020.Day3
open AoC2020.Day4
open AoC2020.Day5

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 3`` () =
        let result = toboccan "test3" 3 1
        Assert.Equal(result, 7)

    [<Fact>]
    let ``day 4`` () =
        let result = countValidPassports "test4" validate
        Assert.Equal(result, 2)

    [<Fact>]
    let ``day 4 valid`` () =
        let result = countValidPassports "valid4" validate2
        Assert.Equal(4, result)

    [<Fact>]
    let ``day 4 invalid`` () =
        let result = countValidPassports "invalid4" validate2
        Assert.Equal(result, 0)

    [<Fact>]
    let ``day 4 fields`` () =
        Assert.True(validateByr "2002")
        Assert.False(validateByr "2003")


        Assert.True(validateHgt "60in")
        Assert.True(validateHgt "190cm")
        Assert.False(validateHgt "190in")
        Assert.False(validateHgt "190")

        Assert.True(validateHcl "#123abc")
        Assert.False(validateHcl "#123abz")
        Assert.False(validateHcl "123abc")

        Assert.True(validateEcl "brn")
        Assert.False(validateEcl "wat")

        Assert.True(validatePid "000000001")
        Assert.False(validatePid "0123456789")

    [<Fact>]
    let ``day 5`` () =
        // BFFFBBFRRR: row 70, column 7, seat ID 567.
        Assert.Equal(567, makeNumber "BFFFBBFRRR")
        Assert.Equal((70, 7), getRowAndCol 567)
        // FFFBBBFRRR: row 14, column 7, seat ID 119.
        Assert.Equal(119, makeNumber "FFFBBBFRRR")
        Assert.Equal((14, 7), getRowAndCol 119)
        // BBFFBBFRLL: row 102, column 4, seat ID 820.
        Assert.Equal(820, makeNumber "BBFFBBFRLL")
        Assert.Equal((102, 4), getRowAndCol 820)
