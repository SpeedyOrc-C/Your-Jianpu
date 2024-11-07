import type {GrPage} from "$lib/your-jianpu/graphical/big/GrPage";
import {GrObjectNode} from "$lib/your-jianpu/graphical/GrObject";

export class GrMusic extends GrObjectNode<GrPage>
{
    constructor(public Pages: GrPage[]) {super();}

    get Children(): GrPage[] { return this.Pages; }
}
