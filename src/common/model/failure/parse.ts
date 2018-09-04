import { Failure as ParsimmonFailure, Index } from 'parsimmon';

import { Failure } from './failure';

export class ParseFailure extends Failure {

  public static fromFailure({ expected, index }: ParsimmonFailure, query: string) {
    return new ParseFailure(expected, index, query);
  }

  constructor(
    expected: string[],
    index: Index,
    public readonly query: string,
  ) {
    super(expected, index);
  }

  public toString() {
    const indent = '    ';

    return 'ParseError in query: \n' +
      `${indent}"""\n${indent}${this.query}\n${indent}"""\n` +
      `${indent}${this.failInfo}`;
  }

}
