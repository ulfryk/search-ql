import { Set } from 'immutable';

import { BinaryOperator } from '../operators';
import { Expression } from './expression';

const BI = 2;

export class BinaryOperationExpression extends Expression {

  constructor(
    public readonly operator: BinaryOperator,
    public readonly value: [Expression, Expression],
  ) {
    super();
    if (value.length !== BI) {
      throw Error(`BinaryOperation has to be made of exactly 2 arguments, not ${value.length}.`);
    }
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof BinaryOperationExpression &&
      this.operator.equals(other.operator) &&
      Set(this.value).equals(Set(other.value)));
  }

  public toString() {
    return this.value.map(expression => expression.toString()).join(` ${this.operator} `);
  }

}
