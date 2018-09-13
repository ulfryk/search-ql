import { ValueType } from '../../../common/model';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class SelectorExpression extends TermExpression<string> {

  public static of(value: string) {
    return (matchingType: ValueType) =>
      new SelectorExpression(matchingType, value);
  }

  constructor(
    public readonly matchingType: ValueType,
    value: string,
  ) {
    super(value, value.trim());
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

}
