import { loadInput } from "../common"

function main(input: string) {
  const result = input
    .split('\n')
    .map((line) => [...line].filter(c => c.match(/\d/)))
    .map((digits) => digits[0] + digits.at(-1))
    .reduce((previous, current) => +current + previous, 0)
  console.log(result)
}

if (require.main === module) {
  main(loadInput())
}
