import { Map } from 'immutable';

import { Expression } from './expression';

export class NotExpression extends Expression {

  constructor(
    public readonly value: Expression,
  ) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof NotExpression &&
      this.value.equals(other.value));
  }

  public toString() {
    return `NOT ${this.value.toString()}`;
  }

  public test(values: Map<string, string>): boolean {
    return !this.value.test(values);
  }

}
