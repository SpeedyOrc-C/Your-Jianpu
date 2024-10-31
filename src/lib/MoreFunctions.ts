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
