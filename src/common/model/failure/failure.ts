import { Failure as ParsimmonFailure, Index } from 'parsimmon';

export abstract class Failure implements Pick<ParsimmonFailure, 'index' | 'expected'> {

  constructor(
    public readonly expected: string[],
    public readonly index: Index,
  ) {}

  public abstract toString(): string;
  public inspect(): string {
    return this.toString();
  }

  protected get failInfo(): string {
    return `- expected ${this.expected.map(e => `"${e}"`).join(' or ')} at ` +
      `line ${this.index.line} column ${this.index.column} (offset: ${this.index.offset})`;
  }

}
