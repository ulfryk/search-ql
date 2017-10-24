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

}
