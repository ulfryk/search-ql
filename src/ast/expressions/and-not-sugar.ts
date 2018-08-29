import { SyntaxConfig } from '../../config';
import { AndOperator } from '../operators';
import { BinaryOperationExpression } from './binary-operation';
import { Expression } from './expression';
import { NotExpression } from './not';

export class AndNotSugarExpression extends Expression {

  public static create({ AND }: SyntaxConfig) {
    return (left: Expression, value: Expression) =>
      new AndNotSugarExpression(new AndOperator(AND[0]), left, value);
  }

  constructor(
    public readonly operator: AndOperator,
    public readonly left: Expression,
    public readonly value: Expression,
  ) { super(); }

  // tslint:disable-next-line:cyclomatic-complexity
  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof AndNotSugarExpression &&
      this.operator.equals(other.operator) &&
      this.left.equals(other.left) &&
      this.value.equals(other.value));
  }

  public rebuild() {
    return new BinaryOperationExpression(this.operator, [
      this.left.rebuild(),
      new NotExpression(this.value.rebuild()),
    ]) as any as this;
  }

  public toString() {
    return `( ${this.left} [AND+NOT] ${this.value} )`;
  }

}
