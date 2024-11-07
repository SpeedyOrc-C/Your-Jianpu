import {GrSVG, SvgId} from "$lib/your-jianpu/graphical/svg-metadata";

export abstract class GrGlyph extends GrSVG
{
    protected constructor() { super([75, 100]); }

    Dump(): string
    {
        const [x, y] = this.AbsolutePosition;

        return `<use x="${x}" y="${y}" href="#${this.SvgId}"/>`;
    }
}

export class GrGlyphX extends GrGlyph
{
    get SvgId(): SvgId { return SvgId.Glyph_X; }
}
