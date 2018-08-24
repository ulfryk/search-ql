import { Set } from 'immutable';

import { Expression } from './expression';
import { MultiaryOperator } from './operators';

export class JoinedExpression extends Expression {

  public static empty(operator: MultiaryOperator) {
    return new JoinedExpression(operator, Set<Expression>());
  }

  constructor(
    public readonly operator: MultiaryOperator,
    public readonly value: Set<Expression>,
  ) { super(); }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof JoinedExpression &&
      this.operator.equals(other.operator) &&
      this.value.equals(other.value));
  }

  public add(operator: MultiaryOperator, expression: Expression): JoinedExpression {
    return this.operator.equals(operator) ?
      new JoinedExpression(this.operator, this.value.add(expression)) :
      new JoinedExpression(operator, Set([this, expression]));
  }

  public toString() {
    return this.value.map(expression => expression.toString()).join(` ${this.operator} `);
  }

}
