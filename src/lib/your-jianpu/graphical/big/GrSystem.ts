import type {GrSystemSlice} from "$lib/your-jianpu/graphical/big/GrSystemSlice";
import {GrObjectNode} from "$lib/your-jianpu/graphical/GrObject";

export class GrSystem extends GrObjectNode<GrSystemSlice>
{
    constructor(public Slices: GrSystemSlice[]) {super();}

    get Children(): GrSystemSlice[] { return this.Slices; }
}
