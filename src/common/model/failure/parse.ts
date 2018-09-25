import { Set } from 'immutable';
import { Maybe } from 'monet';
import { Failure as ParsimmonFailure, Index } from 'parsimmon';

import { Failure } from './failure';

export class ParseFailure extends Failure {

  public static fromFailure({ expected, index }: ParsimmonFailure, query: string) {
    return new ParseFailure(query, expected, index);
  }

  constructor(
    public readonly query: string,
    expected: string[] = [],
    index?: Index, // create class for this one
  ) {
    super(expected, Maybe.fromNull(index));
  }

  // tslint:disable-next-line:cyclomatic-complexity
  public equals(other: Failure): boolean {
    return this === other || (
      other instanceof ParseFailure &&
      Set(this.expected).equals(Set(other.expected)) &&
      this.index
        .flatMap(({ column, line, offset }) =>
          other.index.map(index =>
            column === index.column &&
            line === index.line &&
            offset === index.offset))
        .orJust(true) &&
      this.query === other.query
    );
  }

  public toString() {
    const indent = '    ';

    return 'ParseError in query: \n' +
      `${indent}"""\n${indent}${this.query}\n${indent}"""\n` +
      `${indent}${this.failInfo.orJust('')}`;
  }

}
