import { ValueType } from '../../common/model';
import { Expression } from './expression';

export class NotExpression extends Expression {

  public static of(value: Expression) {
    return new NotExpression(value);
  }

  public static fromParseResult(__: string, operand: Expression) {
    return NotExpression.of(operand);
  }

  public readonly returnType = ValueType.Boolean;

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

  public rebuild() {
    const newValue = this.value.rebuild();

    if (this.value.equals(newValue)) {
      return this;
    }

    return new NotExpression(newValue);
  }

  public toString() {
    return `NOT ${this.value}`;
  }

}
