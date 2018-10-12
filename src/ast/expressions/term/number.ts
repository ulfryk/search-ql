import { Maybe, Some } from 'monet';

import { Expression, ExpressionType, IntegrityFailure, ValueType } from '../../../common/model';
import { isNumber, parseNumber } from '../../../common/utils';
import { InvalidExpression } from '../invalid';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class NumberExpression extends TermExpression {

  public static fromTerm(term: TermExpression) {
    return isNumber(term.value) ? NumberExpression.of(term.value) : term;
  }

  public static of(value: string) {
    return new NumberExpression(value);
  }

  public readonly returnType = ValueType.Number;
  public readonly type: ExpressionType.Number = ExpressionType.Number;

  constructor(value: string) {
    super(value);
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof NumberExpression &&
      parseNumber(this.value) === parseNumber(other.value)
    );
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
    return isNumber(this.value) ? [] : [
      `NumberExpression contains a non-number value: "${this.value}".`,
    ];
  }

}
