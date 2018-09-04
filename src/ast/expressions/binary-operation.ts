import { List, Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { isBooleanType, isSubtype, ValueType } from '../../common/model';
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

  public isValid() {
    return this.value.every(operand => operand.isValid());
  }

  public checkTypes() {
    const [newLeft, newRight] = this.value.map(side => side.checkTypes());

    return this.check(newLeft, newRight)
      .foldLeft(this.clone(newLeft, newRight))(InvalidExpression.fromErrors);
  }

  public reshape() {
    const [newLeft, newRight] = this.value.map(side => side.reshape());

    return this.clone(newLeft, newRight);
  }

  public toString() {
    return this.value.map(String).join(` ${this.operator} `);
  }

  public toList() {
    return List([this])
      .concat(List(this.value)
        .flatMap(operand => operand.toList()))
      .toList();
  }

  private clone(newLeft: Expression, newRight: Expression): Expression {
    const [left, right] = this.value;

    if (left.equals(newLeft) && right.equals(newRight)) {
      return this;
    }

    return new BinaryOperationExpression(this.operator, [newLeft, newRight]);
  }

  private check(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    if (this.operator.is(LikeOperator)) {
      return this.checkLikeTypes(newLeft, newRight);
    }

    if (this.operator.is(AndOperator) || this.operator.is(OrOperator)) {
      return this.checkBooleanTypes(newLeft, newRight);
    }

    return None();
  }

  private checkLikeTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return Some([
      ...this.getLikeSideErrors('L', newLeft.returnType),
      ...this.getLikeSideErrors('R', newRight.returnType),
    ]).filter(errors => errors.length > 0);
  }

  private getLikeSideErrors(side: 'L' | 'R', actual: ValueType): string[] {
    return isSubtype(actual, ValueType.Text) ? [] :
      this.getError(side, actual, ValueType.Text).toList().toArray();
  }

  private checkBooleanTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return Some([
      ...this.getLogicalSideErrors('L', newLeft),
      ...this.getLogicalSideErrors('R', newRight),
    ]).filter(errors => errors.length > 0);
  }

  private getLogicalSideErrors(side: 'L' | 'R', e: Expression): string[] {
    return this.isSideTypeValid(e) ?
      this.getError(side, e.returnType, ValueType.Boolean).toList().toArray() : [];
  }

  private isSideTypeValid(side: Expression): boolean {
    return !side.is(TermExpression as any) && !isBooleanType(side.returnType);
  }

  private getError(side: 'L' | 'R', actual: ValueType, expected: ValueType): Maybe<string> {
    return Some(
      `${side}HS of ${this.operator.token} expression has ` +
      `to be a ${expected} expression, but instead found ${actual}`);
  }

}
