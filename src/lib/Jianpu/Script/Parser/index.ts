import {Parser, ParseString} from "$lib/Parser";

export const ParsePitchBase: Parser<number> =
    Parser.ASum(
        ParseString("1"),
        ParseString("2"),
        ParseString("3"),
        ParseString("4"),
        ParseString("5"),
        ParseString("6"),
        ParseString("7"),
    ).Map(parseInt);
