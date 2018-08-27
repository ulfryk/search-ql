import { Expression } from '../expression';

export class TermExpression extends Expression {

  public static fromMatch(_match: string) {
    throw Error('unimplemented');
  }

  constructor(public readonly value: string) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (other instanceof this.constructor && this.value === other.value);
  }

  public toString() {
    return this.value;
  }

}
