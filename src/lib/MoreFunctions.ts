export function ZipPrevNext<T>(xs: T[]): [T | null, T, T | null][]
{
    if (xs.length == 0)
        return [];

    if (xs.length == 1)
        return [[null, xs[0], null]];

    const first: [T | null, T, T | null] = [null, xs[0], xs[1]];
    const last: [T | null, T, T | null] = [xs.at(-2)!, xs.at(-1)!, null];
    const inner: [T | null, T, T | null][] = [];

    for (let i = 1; i < xs.length - 1; i += 1)
        inner.push([xs[i - 1], xs[i], xs[i + 1]]);

    return [first, ...inner, last];
}

export class SpringInputSSF
{
    constructor(public PreStretchedLength: number, public HooksConstant: number) {}
}

export function SpaceForceFunction(desiredExtent: number, input: SpringInputSSF[]): number
{
    if (input.length == 0)
        throw "Empty input.";

    let minimumExtent = input
        .map(x => x.PreStretchedLength)
        .reduce((x, y) => x + y);

    if (desiredExtent <= minimumExtent)
        return 0;

    const sortedInput = input
        .sort((a, b) =>
            a.HooksConstant * a.PreStretchedLength - b.HooksConstant * b.PreStretchedLength);

    let currentHooksConstant = sortedInput[0].HooksConstant;

    for (let i = 0; i < sortedInput.length; i++)
    {
        const {PreStretchedLength, HooksConstant} = sortedInput[i];

        minimumExtent -= PreStretchedLength;

        const force = (desiredExtent - minimumExtent) / currentHooksConstant;

        if (i == sortedInput.length - 1)
            return force;

        const nextInput = sortedInput[i+1];
        const nextForce = nextInput.PreStretchedLength * nextInput.HooksConstant;

        if (force <= nextForce)
            return force;

        currentHooksConstant = 1 / (1 / currentHooksConstant + 1 / nextInput.HooksConstant)
    }

    throw "This is not possible.";
}
