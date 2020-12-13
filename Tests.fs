module AoC2020.Tests

open Xunit
open Xunit.Abstractions
open AoC2020.Utils
open AoC2020.Day1
open AoC2020.Day2
open AoC2020.Day3
open AoC2020.Day4
open AoC2020.Day5
open AoC2020.Day6
open AoC2020.Day7
open AoC2020.Day8
open AoC2020.Day9
open AoC2020.Day10
open AoC2020.Day11
open AoC2020.Day12
open AoC2020.Day13

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 1`` () =
        Assert.Equal(858496L, day1 "1" ())
        Assert.Equal(263819430L, day1part2 "1" ())

    [<Fact>]
    let ``day 2`` () =
        Assert.Equal(524L, day2 "2" ())
        Assert.Equal(485L, day2part2 "2" ())

    [<Fact>]
    let ``day 3`` () =
        let result = toboccan "test3" 3 1
        Assert.Equal(7L, result)
        Assert.Equal(265L, day3 "3" ())
        Assert.Equal(3154761400L, day3part2 "3" ())

    [<Fact>]
    let ``day 4`` () =
        let result = countValidPassports "test4" validate
        Assert.Equal(2L, result)
        Assert.Equal(260L, day4 "4" ())
        Assert.Equal(153L, day4part2 "4" ())

    [<Fact>]
    let ``day 4 valid`` () =
        let result = countValidPassports "valid4" validate2
        Assert.Equal(4L, result)

    [<Fact>]
    let ``day 4 invalid`` () =
        let result = countValidPassports "invalid4" validate2
        Assert.Equal(0L, result)

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

        Assert.Equal(850L, day5 "5" ())
        Assert.Equal(599L, day5part2 "5" ())

    [<Fact>]
    let ``day 6`` () =
        Assert.Equal(11L, readInputDelimByEmptyLine "test6" |> countAnswers)
        Assert.Equal(6L, day6part2 "test6b" ())
        Assert.Equal(6506L, day6 "6" ())
        Assert.Equal(3243L, day6part2 "6" ())
        ()
        
    [<Fact>]
    let ``day 7`` () =
        Assert.Equal(4L, day7 "test7" ())
        Assert.Equal(289L, day7 "7" ())
        Assert.Equal(32L, day7part2 "test7" ())
        Assert.Equal(126L, day7part2 "test7b" ())
        Assert.Equal(30055L, day7part2 "7" ())
        

    [<Fact>]
    let ``day 8`` () =
        Assert.Equal(5L, day8 "test8" ())
        Assert.Equal(1451L, day8 "8" ())
        Assert.Equal(8L, day8part2 "test8" ())
        Assert.Equal(1160L, day8part2 "8" ())

    [<Fact>]
    let ``day 9`` () =
        Assert.Equal(127L, day9 "test9" 5 ())
        Assert.Equal(393911906L, day9 "9" 25 ())
        Assert.Equal(62L, day9part2 "test9" 5 ())
        
    [<Fact>]
    let ``day10`` () =
        Assert.Equal(35L, day10 "test10" ())
        Assert.Equal(220L, day10 "test10b" ())
        Assert.Equal(1885L, day10 "10" ())
    
    [<Fact>]
    let ``day10 part 2`` () =
        Assert.Equal(8L, day10part2 "test10" ())
        Assert.Equal(19208L, day10part2 "test10b" ())
        Assert.Equal(2024782584832L, day10part2 "10" ())

    [<Fact>]
    let ``day 11`` () =
        Assert.Equal(37L, day11 "test11" "1" ())
        Assert.Equal(2152L, day11 "11" "1" ())

    [<Fact>]
    let ``day 11 part 2 visibility algorithm`` () =
        Assert.Equal(8L, testVisibility "test11b1" 4 3 ())
        Assert.Equal(0L, testVisibility "test11b2" 1 1 ())
        Assert.Equal(0L, testVisibility "test11b3" 3 3 ())
        
    [<Fact>]
    let ``day 12`` () =
        Assert.Equal(25L, day12 "test12" ())
        Assert.Equal(759L, day12 "12" ())

    [<Fact>]
    let ``day 13`` () =
        Assert.Equal(295L, day13 "test13" ())
        Assert.Equal(4782L, day13 "13" ())

    [<Fact>]
    let ``day 13 part 2`` () =
        Assert.Equal(1068781L, solvePart2 "7,13,x,x,59,x,31,19") 
        Assert.Equal(3417L, solvePart2 "17,x,13,19")
        Assert.Equal(754018L, solvePart2 "67,7,59,61")
        Assert.Equal(779210L, solvePart2 "67,x,7,59,61" )
        Assert.Equal(1261476L, solvePart2 "67,7,x,59,61")
        Assert.Equal(1202161486L, solvePart2 "1789,37,47,1889")
        
