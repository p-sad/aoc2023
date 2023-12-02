import { readFileSync } from "fs"

export function loadInput(): string {
  const filename = process.argv.at(-1)
  if (!filename) return ''
  return readFileSync(filename, { encoding: 'utf8' })
}
