import { Set } from 'immutable';

import { Expression } from './expression';

export class JoinedExpression extends Expression {

  public static empty(operator: string) {
    return new JoinedExpression(operator, Set<Expression>());
  }

  constructor(
    public readonly type: string,
    public readonly value: Set<Expression>,
  ) { super(); }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof JoinedExpression &&
      this.type === other.type &&
      this.value.equals(other.value));
  }

  public add(key: string, expression: Expression): JoinedExpression {
    return this.type === key ?
      new JoinedExpression(this.type, this.value.add(expression)) :
      new JoinedExpression(key, Set([this, expression]));
  }

  public toString() {
    return this.value.map(expression => expression.toString()).join(` ${this.type} `);
  }

}
