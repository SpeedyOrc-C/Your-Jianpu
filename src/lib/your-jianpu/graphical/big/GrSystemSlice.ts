import type {GrStaff} from "$lib/your-jianpu/graphical/big/GrStaff";
import {GrObjectNode} from "$lib/your-jianpu/graphical/GrObject";

export class GrSystemSlice extends GrObjectNode<GrStaff>
{
    constructor(public Staves: GrStaff[]) {super();}

    get Children(): GrStaff[] { return this.Staves; }
}
