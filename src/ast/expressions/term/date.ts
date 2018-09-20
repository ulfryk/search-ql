import { Maybe, Some } from 'monet';

import { Expression, ExpressionType, ValueType } from '../../../common/model';
import { InvalidExpression } from '../invalid';
import { isDate } from './is-date';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class DateExpression extends TermExpression<number> {

  public static fromTerm(term: TermExpression) {
    return isDate(term.value) ? DateExpression.of(term.value) : term;
  }

  public static of(value: string) {
    return new DateExpression(value);
  }

  // TODO: probably use config here
  public static prepareValue(value: string): number {
    return Date.parse(value.trim());
  }

  public readonly returnType = ValueType.Date;
  public readonly type = ExpressionType.Date;

  constructor(value: string, preparedValue = DateExpression.prepareValue(value)) {
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
      ...this.getNonNumberPreparedValueError(),
      ...this.getNotMatchingValuesError(),
    ]).filter(errors => errors.length > 0);
  }

  private getWrongValueError() {
    return isDate(this.value) ? [] : [
      `DateExpression contains a non-date value: "${this.value}".`,
    ];
  }

  private getNonNumberPreparedValueError() {
    return Number(this.preparedValue) === this.preparedValue ? [] : [
      `DateExpression contains a non-number preparedValue: ${this.preparedValue}.`,
    ];
  }

  private getNotMatchingValuesError() {
    return DateExpression.prepareValue(this.value) === this.preparedValue ? [] : [
      `DateExpression value ("${this.value}") doesn't match ` +
        `preparedValue ("${this.preparedValue}").`,
    ];
  }

}
