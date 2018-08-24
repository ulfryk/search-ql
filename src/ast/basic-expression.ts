// tslint:disable-next-line:no-import-side-effect
import '@samwise-tech/immutable/Iterable/lastMaybe';

import { Expression } from './expression';

export class BasicExpression extends Expression {

  public static fromMatch(match: string) {
    return new BasicExpression(match);
  }

  public readonly value: string;

  constructor(value: string) {
    super();
    this.value = value.toLowerCase();
  }

  public equals(other: Expression): boolean {
    return this === other || (other instanceof BasicExpression && this.value === other.value);
  }

  public toString() {
    return `"${this.value}"`;
  }

}
