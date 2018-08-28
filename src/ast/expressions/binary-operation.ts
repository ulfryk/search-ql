import { Set } from 'immutable';

import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from '../operators';
import { Expression } from './expression';
import { SelectorExpression, TermExpression, TextExpression } from './term';

const BI = 2;

export class BinaryOperationExpression extends Expression {

  public static and(token: string) {
    return (lhs: Expression, rhs: Expression) =>
      new BinaryOperationExpression(new AndOperator(token), [lhs, rhs]);
  }

  public static fromPair(operator: BinaryOperator) {
    // tslint:disable-next-line:cyclomatic-complexity
    return (lhs: Expression, rhs: Expression) => {

      if (operator.is(LikeOperator)) {

        // TODO: Use Validation for syntax errors
        if (!lhs.is(TermExpression as any /* TypeScript, why? */ as typeof Expression)) {
          throw SyntaxError('LHS of LIKE expression has to be a term expression');
        }

        // TODO: Use Validation for syntax errors
        if (!rhs.is(TermExpression as any /* TypeScript, why? */ as typeof Expression)) {
          throw SyntaxError('RHS of LIKE expression has to be a term expression');
        }

        return new BinaryOperationExpression(operator, [
          SelectorExpression.fromTerm(lhs as TermExpression),
          TextExpression.fromTerm(rhs as TermExpression),
        ]);
      }

      if (operator.is(AndOperator) || operator.is(OrOperator)) {
        return new BinaryOperationExpression(operator, [
          lhs.is(TermExpression as any /* TypeScript, why? */ as typeof Expression) ?
            TextExpression.fromTerm(lhs as TermExpression) : lhs,
          rhs.is(TermExpression as any /* TypeScript, why? */ as typeof Expression) ?
            TextExpression.fromTerm(rhs as TermExpression) : rhs,
        ]);
      }

      return new BinaryOperationExpression(operator, [lhs, rhs]);
    };
  }

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
    return this.value.map(String).join(` ${this.operator} `);
  }

}
