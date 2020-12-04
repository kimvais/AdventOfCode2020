module AoC2020.Tests

open Xunit
open Xunit.Abstractions
open AoC2020.Day3
open AoC2020.Day4

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 3`` () =
        let result = toboccan "test3" 3 1
        Assert.Equal(result, 7)

    [<Fact>]
    let ``day 4`` () =
        let result = validatePassports validate "test4"
        Assert.Equal(result, 2)

    [<Fact>]
    let ``day 4 valid`` () =
        let result = validatePassports validate2 "valid4"
        Assert.Equal(4, result)

    [<Fact>]
    let ``day 4 invalid`` () =
        let result = validatePassports validate2 "invalid4"
        Assert.Equal(result, 0)

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
