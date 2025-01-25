#!/usr/bin/env node

import puppeteer from "puppeteer"

const [_1, _2, fontSize, font, ...texts] = process.argv
const browser = await puppeteer.launch()
const page = await browser.newPage()

await page.goto(`file://${process.cwd()}/template.html`)

await page.evaluate(`
    const span = document.getElementById("target-span");
    span.style.fontSize = "${fontSize}px";
    span.style.fontFamily = "${font}"
`)

const callStrings = texts.map(text => `span.innerText = "${text}";JSON.stringify(span.getClientRects()[0])`)
const rawMetrics = await Promise.all(callStrings.map(x => page.evaluate(x)))
const metrics = rawMetrics.map(JSON.parse)
const output = metrics.map(({width, height}) => `${width}*${height}`).join(";")

await browser.close()

console.log(output)
