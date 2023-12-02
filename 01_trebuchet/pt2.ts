import { loadInput } from "../common"

function main(input: string) {
  const result = input
    .split('\n')
    .map((line) => [...line.matchAll(pattern)]
      .map((match) => match[0])
      .map((token) => wordToNumber[token] ?? token))
    .map((digits) => +(digits[0] + digits.at(-1)))
    .reduce((prev, current) => prev + current, 0)
  console.log(result)
}

const pattern = /\d|one|two|three|four|five|six|seven|eight|nine/g

const wordToNumber = {
  one: '1', two: '2', three: '3', four: '4', five: '5', six: '6', seven: '7', eight: '8', nine: '9',
}

if (require.main === module) {
  main(loadInput())
}
