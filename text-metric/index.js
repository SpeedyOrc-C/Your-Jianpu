#!/usr/bin/env node

import puppeteer from "puppeteer"

const [_1, _2, fontSize, font, ...texts] = process.argv
const browser = await puppeteer.launch()
const page = await browser.newPage()

await page.goto(`file://${process.cwd()}/template.svg`)

const style = `
@font-face { font-family: my-font; src: url("${font}"); }
text { font-size: ${fontSize}px; }
`

await page.evaluate(`
document.querySelector("style").innerHTML = \`${style}\`
const text = document.querySelector("text")
text.style.fontSize = "${fontSize}px"
`)

const metrics = []

for (const text of texts)
{
    await page.evaluate(`text.textContent = \`${text}\``)
    const clientRectsRaw = await page.evaluate(`JSON.stringify(text.getClientRects())`)
    const clientRects = JSON.parse(clientRectsRaw)
    const {width, height} = clientRects[0]
    metrics.push({width, height})
}

await browser.close()

const output = metrics.map(({width, height}) => `${width}*${height}`).join("\n")

// console.log(JSON.stringify(metrics))
console.log(output)
