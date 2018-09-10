import { ValueType } from '../../../common/model';
import { TermExpression } from './term';

export class PhraseExpression extends TermExpression<string> {

  public static fromTerm({ value }: TermExpression) {
    return PhraseExpression.of(value);
  }

  public static of(value: string) {
    return new PhraseExpression(value);
  }

  public readonly returnType: ValueType = ValueType.Phrase;

  constructor(value: string) {
    super(value, value.trim());
  }

}
