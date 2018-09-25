import { ISetoid } from '@samwise-tech/core';
import { Maybe, None } from 'monet';
import { Index } from 'parsimmon';

export abstract class Failure implements ISetoid {

  constructor(
    public readonly expected: string[],
    public readonly index: Maybe<Index> = None(),
  ) {}

  public abstract equals(other: Failure): boolean;

  public abstract toString(): string;
  public inspect(): string {
    return this.toString();
  }

  protected get failInfo(): Maybe<string> {
    return this.index.map(({ column, line, offset }) =>
      `- expected ${this.expected.map(e => `"${e}"`).join(' or ')} at ` +
        `line ${line} column ${column} (offset: ${offset})`);
  }

}
