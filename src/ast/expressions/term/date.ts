import { Maybe, Some } from 'monet';

import { Expression, ExpressionType, IntegrityFailure, TimeFrame, ValueType } from '../../../common/model';
import { isDate, parseDate } from '../../../common/utils';
import { IDateExpression } from '../../../dto';
import { InvalidExpression } from '../invalid';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class DateExpression extends TermExpression {

  public static fromTerm(term: TermExpression) {
    return isDate(term.value) ? DateExpression.of(term.value) : term;
  }

  public static of(value: string) {
    return new DateExpression(value);
  }

  public readonly returnType = ValueType.Date;
  public readonly type: ExpressionType.Date = ExpressionType.Date;

  constructor(
    value: string,
    public readonly timeFrame: TimeFrame = parseDate(value),
  ) {
    super(value);
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof DateExpression &&
      this.timeFrame.equals(other.timeFrame)
    );
  }

  public toJS(): IDateExpression {
    return {
      returnType: this.returnType,
      timeFrame: this.timeFrame.toJS(),
      type: this.type,
      value: this.value,
    };
  }

  public checkIntegrity(): Expression {
    return this.getIntegrityErrors()
      .map(errors => errors.map(IntegrityFailure.fromError(this)))
      .foldLeft(this as Expression)(InvalidExpression.fromErrors);
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

  private getIntegrityErrors(): Maybe<string[]> {
    return Some([
      ...this.getWrongValueError(),
    ]).filter(errors => errors.length > 0);
  }

  private getWrongValueError() {
    return isDate(this.value) ? [] : [
      `DateExpression contains a non-date value: "${this.value}".`,
    ];
  }

}
