import type {GrSystem} from "$lib/your-jianpu/graphical/big/GrSystem";
import {GrObjectNode} from "$lib/your-jianpu/graphical/GrObject";
import {SvgFromId, type SvgId} from "$lib/your-jianpu/graphical/svg-metadata";

export class GrPage extends GrObjectNode<GrSystem>
{
    constructor(public Systems: GrSystem[]) {super();}

    get Children(): GrSystem[] { return this.Systems }

    Write(write: (s: string) => void): void
    {
        // const [[x1, y1], [x2, y2]] = this.BoundingBox;
        //
        // write(`<svg viewBox="${x1} ${y1} ${x2} ${y2}" xmlns="http://www.w3.org/2000/svg">`)
        // write(`<defs>`);
        //
        // for (const usedSvgId of svgUsage)
        // {
        //     const svg = SvgFromId.get(usedSvgId);
        //
        //     if (svg == undefined)
        //         throw `SVG "${usedSvgId}" is missing.`;
        //
        //     write(svg);
        // }
        //
        // write(`</defs>`);
        //
        // write(`</svg>`);
    }
}
