module AoC2020.Tests

open Xunit
open Xunit.Abstractions
open AoC2020.Day3

type Puzzles(o:ITestOutputHelper)=
    [<Fact>]
    let ``day 3`` () =
        let result = toboccan "test3" 3 1
        Assert.Equal(result, 7)