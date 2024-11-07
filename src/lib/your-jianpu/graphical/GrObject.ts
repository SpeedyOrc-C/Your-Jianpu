import {SvgId} from "./svg-metadata";

export type XY = [number, number];

export abstract class GrObject<TChild extends GrObject<any>>
{
    protected constructor(
        public Position: XY = [0, 0],
        public Parent: GrObjectNode<GrObject<TChild>> | null = null)
    {}

    protected AssignThisParentToChildren(): void
    {
        for (const child of this.Children)
            child.Parent = this;
    }

    get X(): number { return this.Position[0]; }

    get Y(): number { return this.Position[1]; }

    set X(x: number) { this.Position[0] = x; }

    set Y(y: number) { this.Position[1] = y; }

    AppendSvgChildrenTo(records: GrSvg[]): void
    {
        for (const child of this.Children)
            if (child instanceof GrSvg)
                records.push(child);
            else
                child.AppendSvgChildrenTo(records);
    }

    get ParentChain(): GrObjectNode<any>[]
    {
        if (this.Parent == null)
            return [];

        return [this.Parent, ...this.Parent.ParentChain];
    }

    get AbsolutePosition(): [number, number]
    {
        if (this.Parent == null)
            return this.Position;

        const [parentX, parentY] = this.Parent.AbsolutePosition;

        return [parentX + this.X, parentY + this.Y];
    }

    abstract get Children(): TChild[];

    abstract get BoundingBox(): [XY, XY];
}

export class GrObjectLeaf extends GrObject<any>
{
    protected constructor(public Size: XY, position: XY = [0, 0])
    {
        super(position);
    }

    get Children(): never[] { return []; }

    get BoundingBox(): [XY, XY]
    {
        return [[0, 0], [this.Size[0], this.Size[1]]];
    }
}

export abstract class GrObjectNode<TChild extends GrObject<any>>
    extends GrObject<TChild>
{
    abstract get Children(): TChild[];

    protected constructor()
    {
        super();
    }

    get BoundingBox(): [XY, XY]
    {
        let minX: number = Number.MAX_VALUE;
        let minY: number = Number.MAX_VALUE;
        let maxX: number = Number.MIN_VALUE;
        let maxY: number = Number.MIN_VALUE;

        const children: TChild[] = this.Children;

        if (children.length == 0)
            throw "No children, cannot determine bounding box.";

        for (const child of children)
        {
            const [[childMinX, childMinY], [childMaxX, childMaxY]] = child.BoundingBox;

            const newMinX: number = child.X + childMinX;
            const newMinY: number = child.Y + childMinY;
            const newMaxX: number = child.X + childMaxX;
            const newMaxY: number = child.Y + childMaxY;

            if (newMinX < minX) minX = newMinX;
            if (newMinY < minY) minY = newMinY;
            if (newMaxX > maxX) maxX = newMaxX;
            if (newMaxY > maxY) maxY = newMaxY;
        }

        return [[minX, minY], [maxX, maxY]];
    }
}

export abstract class GrSvg extends GrObjectLeaf
{
    abstract get Id(): SvgId;
    abstract get Width(): number;
    abstract get Height(): number;

    constructor(position: XY)
    {
        super([0, 0], position);

        this.PostConstructor();
    }

    private PostConstructor()
    {
        this.Size[0] = this.Width;
        this.Size[1] = this.Height;
    }
}

export function ShowGrSvg(svg: GrSvg): string
{
    const [x, y] = svg.AbsolutePosition;
    return `<use x="${x}" y="${y}" href="#${svg.Id}"/>`;
}
