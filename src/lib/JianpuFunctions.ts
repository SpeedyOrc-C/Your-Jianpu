
// export function GroupByBeaming(codePoints: LineElement[], multiplier: Multiplier): LineElement[][]
// {
//     console.assert(codePoints.length >= 1);
//
//     const result: LineElement[][] = [];
//
//     let temporaryGroup: LineElement[] = [];
//     let accumulation = 0;
//
//     for (const codePoint of codePoints)
//     {
//         const duration = codePoint.Duration();
//
//         accumulation += duration;
//
//         if (accumulation >= multiplier)
//         {
//             accumulation %= multiplier;
//             temporaryGroup.push(codePoint);
//             result.push(temporaryGroup);
//             temporaryGroup = [];
//         } else
//         {
//             temporaryGroup.push(codePoint);
//         }
//     }
//
//     return result;
// }

