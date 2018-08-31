import { Failure, Index } from 'parsimmon';

export class ParseFailure implements Failure {

  public static fromFailure({ expected, index }: Failure, query: string) {
    return new ParseFailure(expected, index, query);
  }

  public readonly status: false = false;

  constructor(
    public readonly expected: string[],
    public readonly index: Index,
    public readonly query: string,
  ) {}

  public toString() {
    const indent = '    ';

    return `ParseError in query: \n${indent}"""\n${indent}${this.query}\n${indent}"""\n` +
      `${indent}- expected ${this.expected.map(e => `"${e}"`).join(' or ')} at ` +
      `line ${this.index.line} column ${this.index.column} (offset: ${this.index.offset})`;
  }

}
