import { List, Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { checkBoolCompatibility, Expression, isBooleanType, isPhraseType, isSubtype, ValueType } from '../../common/model';
import { AndOperator, BinaryOperator, EqualityOperator, LogicalOperator } from '../operators';
import { InvalidExpression } from './invalid';
import { PhraseExpression, SelectorExpression, TermExpression, TextExpression } from './term';

const BI = 2;

export class BinaryOperationExpression extends Expression {

  public static and(token: string) {
    return (lhs: Expression, rhs: Expression) =>
      BinaryOperationExpression.fromPair(new AndOperator(token))(lhs, rhs);
  }

  public static fromPair(operator: BinaryOperator) {
    // tslint:disable-next-line:cyclomatic-complexity
    return (lhs: Expression, rhs: Expression) => {

      if (operator.is(EqualityOperator)) {
        if (lhs.is(SelectorExpression as any)) {
          return new BinaryOperationExpression(operator, [
            lhs,
            rhs.is(TermExpression as any) ? PhraseExpression.fromTerm(rhs as TermExpression) : rhs,
          ]);
        }
        return new BinaryOperationExpression(operator, [
          lhs,
          rhs.is(SelectorExpression as any) ?
            TextExpression.fromTerm(rhs as SelectorExpression) : rhs,
        ]);
      }

      if (operator.is(LogicalOperator)) {
        return new BinaryOperationExpression(operator, [
          lhs.is(TermExpression as any) ? PhraseExpression.fromTerm(lhs as TermExpression) : lhs,
          rhs.is(TermExpression as any) ? PhraseExpression.fromTerm(rhs as TermExpression) : rhs,
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

  public get returnType(): ValueType {
    return this.value.some(({ returnType }) => isPhraseType(returnType)) ?
      ValueType.Phrase : ValueType.Boolean;
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
    if (this.operator.is(EqualityOperator)) {
      return this.checkEqualityTypes(newLeft, newRight);
    }

    if (this.operator.is(LogicalOperator)) {
      return this.checkBooleanTypes(newLeft, newRight);
    }

    return None();
  }

  // Eq operators type checking

  private checkEqualityTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    if (newLeft.is(SelectorExpression as any)) {
      return this.checkSelectorEqualityTypes(newLeft, newRight);
    }
    return this.checkRegularEqualityTypes(newLeft, newRight);
  }

  private checkRegularEqualityTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return this.areCompatible(newLeft, newRight) ? None() : Some([
      `Both sides of ${this.operator.token} expression should be of same type, but got ` +
      `LHS: ${newLeft.returnType} and RHS: ${newRight.returnType}.`,
    ]);
  }

  private areCompatible(newLeft: Expression, newRight: Expression) {
    return newLeft.returnType === newRight.returnType ||
      checkBoolCompatibility(newLeft.returnType, newRight.returnType);
  }

  private checkSelectorEqualityTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return Some([
      ...this.getEqualitySideErrors('L', newLeft.returnType, ValueType.Text),
      ...this.getEqualitySideErrors('R', newRight.returnType, ValueType.Phrase),
    ]).filter(errors => errors.length > 0);
  }

  private getEqualitySideErrors(side: 'L' | 'R', actual: ValueType, superType: ValueType): string[] {
    return isSubtype(actual, superType) ? [] :
      this.getError(side, actual, [superType]).toList().toArray();
  }

  // Logical operators type checking

  private checkBooleanTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return Some([
      ...this.getLogicalSideErrors('L', newLeft),
      ...this.getLogicalSideErrors('R', newRight),
    ]).filter(errors => errors.length > 0);
  }

  private getLogicalSideErrors(side: 'L' | 'R', e: Expression): string[] {
    return this.isSideTypeValid(e) ? [] :
    this.getError(side, e.returnType, [ValueType.Boolean, ValueType.Phrase]).toList().toArray();
  }

  private isSideTypeValid({ returnType }: Expression): boolean {
    return isBooleanType(returnType) || isPhraseType(returnType);
  }

  private getError(side: 'L' | 'R', actual: ValueType, expected: ValueType[]): Maybe<string> {
    return Some(
      `${side}HS of ${this.operator.token} expression has ` +
      `to be a ${expected.join(' or ')} expression, but instead found ${actual}`);
  }

}
