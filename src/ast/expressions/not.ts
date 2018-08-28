import { Expression } from './expression';

export class NotExpression extends Expression {

  public static of(value: Expression) {
    return new NotExpression(value);
  }

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
    return `NOT ${this.value}`;
  }

}
