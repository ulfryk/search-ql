import { Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { TEXT_TYPES, ValueType } from '../../common/model';
import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from '../operators';
import { Expression } from './expression';
import { InvalidExpression } from './invalid';
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
        return new BinaryOperationExpression(operator, [
          lhs.is(TermExpression as any) ? SelectorExpression.fromTerm(lhs as any) : lhs,
          rhs.is(TermExpression as any) ? TextExpression.fromTerm(rhs as any) : rhs,
        ]);
      }

      if (operator.is(AndOperator) || operator.is(OrOperator)) {
        return new BinaryOperationExpression(operator, [
          lhs.is(TermExpression as any) ? TextExpression.fromTerm(lhs as any) : lhs,
          rhs.is(TermExpression as any) ? TextExpression.fromTerm(rhs as any) : rhs,
        ]);
      }

      return new BinaryOperationExpression(operator, [lhs, rhs]);
    };
  }

  public readonly returnType = ValueType.Boolean;

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

  public checkTypes() {
    const [newLeft, newRight] = this.value.map(side => side.checkTypes());

    return this.check(newLeft, newRight)
      .foldLeft(this.clone(newLeft, newRight))(InvalidExpression.fromError);
  }

  public reshape() {
    const [newLeft, newRight] = this.value.map(side => side.reshape());

    return this.clone(newLeft, newRight);
  }

  public toString() {
    return this.value.map(String).join(` ${this.operator} `);
  }

  private clone(newLeft: Expression, newRight: Expression): Expression {
    const [left, right] = this.value;

    if (left.equals(newLeft) && right.equals(newRight)) {
      return this;
    }

    return new BinaryOperationExpression(this.operator, [newLeft, newRight]);
  }

  private check(newLeft: Expression, newRight: Expression): Maybe<string> {
    if (this.operator.is(LikeOperator)) {
      return this.checkLikeTypes(newLeft, newRight);
    }

    if (this.operator.is(AndOperator) || this.operator.is(OrOperator)) {
      return this.checkBooleanTypes(newLeft, newRight);
    }

    return None();
  }

  private checkLikeTypes(newLeft: Expression, newRight: Expression): Maybe<string> {
    if (!TEXT_TYPES.some(type => type === newLeft.returnType)) {
      return this.getError('L', newLeft.returnType, TEXT_TYPES);
    }

    if (!TEXT_TYPES.some(type => type === newRight.returnType)) {
      return this.getError('R', newLeft.returnType, TEXT_TYPES);
    }

    return None();
  }

  private checkBooleanTypes(newLeft: Expression, newRight: Expression): Maybe<string> {
    if (this.isSideTypeValid(newLeft)) {
      return this.getError('L', newLeft.returnType, [ValueType.Boolean]);
    }

    if (this.isSideTypeValid(newRight)) {
      return this.getError('R', newRight.returnType, [ValueType.Boolean]);
    }

    return None();
  }

  private isSideTypeValid(side: Expression): boolean {
    return !side.is(TermExpression as any) && side.returnType !== ValueType.Boolean;
  }

  private getError(side: 'L' | 'R', actual: ValueType, expected: ReadonlyArray<ValueType>): Maybe<string> {
    return Some(
      `${side}HS of ${this.operator.token} expression has ` +
      `to be a ${expected.join('/')} expression, but instead found ${actual}`);
  }

}
