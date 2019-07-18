const fs = require('fs')
const path = require('path')

const { main } = require('./output/Main')

const INPUT_EXT = '.c'
const OUTPUT_EXT = '.s'

const entry = process.argv[2]
if (!entry) {
  throw new Error('No entry!')
}
const entryPath = path.join(__dirname, entry)
if (path.extname(entryPath) !== INPUT_EXT) {
  throw new Error(`File extension must be ${INPUT_EXT}.`)
}
const outPath = path.format({
  dir: process.cwd(),
  name: path.basename(entryPath, INPUT_EXT),
  ext: OUTPUT_EXT
})
console.log(`Input: ${entryPath}`)

// Read
const input = fs.readFileSync(entryPath).toString()

// Compile
const output = main(input)

// Write
fs.writeFileSync(outPath, output)

console.log(`Output: ${outPath}`)
