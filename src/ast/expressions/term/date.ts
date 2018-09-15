import { ExpressionType, ValueType } from '../../../common/model';
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

  constructor(value: string) {
    super(value, DateExpression.prepareValue(value));
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

}
