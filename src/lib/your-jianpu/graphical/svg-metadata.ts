import Accidental_Flat from "$lib/svg/Accidental_Flat.svg?raw";
import Accidental_Natural from "$lib/svg/Accidental_Natural.svg?raw";
import Accidental_Sharp from "$lib/svg/Accidental_Sharp.svg?raw";
import Glyph_0 from "$lib/svg/Glyph_0.svg?raw";
import Glyph_1 from "$lib/svg/Glyph_1.svg?raw";
import Glyph_X from "$lib/svg/Glyph_X.svg?raw";
import Underline from "$lib/svg/Underline.svg?raw";
import SmallDot from "$lib/svg/SmallDot.svg?raw";

export enum SvgId
{
    Accidental_Flat = "Accidental_Flat",
    Accidental_Natural = "Accidental_Natural",
    Accidental_Sharp = "Accidental_Sharp",
    Glyph_0 = "Glyph_0",
    Glyph_1 = "Glyph_1",
    Glyph_X = "Glyph_X",
    Underline = "Underline",
    SmallDot = "SmallDot",
}

export const SvgFromId: Map<SvgId, string> = new Map([
    [SvgId.Accidental_Flat, Accidental_Flat],
    [SvgId.Accidental_Natural, Accidental_Natural],
    [SvgId.Accidental_Sharp, Accidental_Sharp],
    [SvgId.Glyph_0, Glyph_0],
    [SvgId.Glyph_1, Glyph_1],
    [SvgId.Glyph_X, Glyph_X],
    [SvgId.Underline, Underline],
    [SvgId.SmallDot, SmallDot],
]);
