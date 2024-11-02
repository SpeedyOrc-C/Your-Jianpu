export class Parser<A>
{
    constructor(public readonly Run: (s: string) => [A, string]) {}

    Map<B>(f: (a: A) => B): Parser<B>
    {
        const p: (s: string) => [A, string] = this.Run;

        return new Parser<B>(function (s: string): [B, string]
        {
            const [a, tail] = p(s);
            return [f(a), tail];
        });
    }

    Bind<B>(f: (a: A) => Parser<B>): Parser<B> // >>=
    {
        const pa: (s: string) => [A, string] = this.Run;

        return new Parser<B>(function (s: string): [B, string]
        {
            const [a, t1] = pa(s);
            const pb: Parser<B> = f(a);
            return pb.Run(t1);
        });
    }

    static Pure<A>(a: A): Parser<A> // pure/return
    {
        return new Parser((s: string): [A, string] => [a, s]);
    }

    Or(p2: Parser<A>): Parser<A> // <|>
    {
        const pa: (s: string) => [A, string] = this.Run;

        return new Parser(function (s: string): [A, string]
        {
            try
            {
                return pa(s);
            }
            catch
            {
                return p2.Run(s);
            }
        });
    }

    Some(): Parser<[A, ...A[]]> // some
    {
        const p: (s: string) => [A, string] = this.Run;

        return new Parser(function (s: string): [[A, ...A[]], string]
        {
            const [first, tail1] = p(s);

            const result: A[] = [];
            let tail2: string = tail1;

            while (true)
            {
                try
                {
                    const [a, newTail2] = p(tail2);
                    result.push(a);
                    tail2 = newTail2;
                }
                catch
                {
                    if (result.length == 0)
                        throw "At least one element required."

                    return [[first, ...result], tail2];
                }
            }
        });
    }

    Many(): Parser<A[]> // many
    {
        const p: (s: string) => [A, string] = this.Run;

        return new Parser(function (s: string): [A[], string]
        {
            const result: A[] = [];
            let tail: string = s;

            while (true)
            {
                try
                {
                    const [a, newTail] = p(tail);
                    result.push(a);
                    tail = newTail;
                }
                catch
                {
                    return [result, tail];
                }
            }
        });
    }

    static Empty(): Parser<unknown> // empty
    {
        return new Parser((_: string) =>
        {
            throw "Empty parser";
        });
    }

    static ASum<A>(...ps: Parser<A>[]): Parser<A> // asum
    {
        return new Parser(function (s: string)
        {
            for (const p of ps)
            {
                try
                {
                    return p.Run(s);
                }
                catch
                {}
            }

            throw "Parser Failed";
        });
    }
}

export function ParseString(s: string): Parser<string>
{
    return new Parser(function (s2)
    {
        if (s2.startsWith(s))
            return [s, s2.substring(s.length)]
        throw "Parser Failed";
    });
}
