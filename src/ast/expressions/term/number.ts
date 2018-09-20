import { Maybe, Some } from 'monet';

import { Expression, ExpressionType, ValueType } from '../../../common/model';
import { InvalidExpression } from '../invalid';
import { isNumber } from './is-number';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class NumberExpression extends TermExpression<number> {

  public static fromTerm(term: TermExpression) {
    return isNumber(term.value) ? NumberExpression.of(term.value) : term;
  }

  public static of(value: string) {
    return new NumberExpression(value);
  }

  // TODO: probably use config here
  public static prepareValue(value: string): number {
    return Number(value.trim());
  }

  public readonly returnType = ValueType.Number;
  public readonly type = ExpressionType.Number;

  constructor(value: string, preparedValue = NumberExpression.prepareValue(value)) {
    super(value, preparedValue);
  }

  public checkIntegrity(): Expression {
    return this.getIntegrityErrors()
      .foldLeft(this as Expression)(InvalidExpression.fromErrors);
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

  private getIntegrityErrors(): Maybe<string[]> {
    return Some([
      ...this.getWrongValueError(),
      ...this.getWrongPreparedValueError(),
      ...this.getNotMatchingValuesError(),
    ]).filter(errors => errors.length > 0);
  }

  private getWrongValueError() {
    return isNumber(this.value) ? [] : [
      `NumberExpression contains a non-number value: "${this.value}".`,
    ];
  }

  private getWrongPreparedValueError() {
    return Number(this.preparedValue) === this.preparedValue ? [] : [
      `NumberExpression contains a non-number preparedValue: "${this.preparedValue}" ` +
        `(${typeof this.preparedValue}).`,
    ];
  }

  private getNotMatchingValuesError() {
    return NumberExpression.prepareValue(this.value) === Number(this.preparedValue) ? [] : [
      `NumberExpression value ("${this.value}") doesn't match ` +
        `preparedValue ("${this.preparedValue}").`,
    ];
  }

}
