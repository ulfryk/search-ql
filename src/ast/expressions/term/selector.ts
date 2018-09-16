import { ExpressionType, ValueType } from '../../../common/model';
import { ISelectorExpression } from '../../../dto';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class SelectorExpression extends TermExpression<string> {

  public static of(value: string) {
    return (matchingType: ValueType) =>
      new SelectorExpression(matchingType, value);
  }

  public readonly type = ExpressionType.Selector;

  constructor(
    public readonly matchingType: ValueType,
    value: string,
    preparedValue = value.trim(),
  ) {
    super(value, preparedValue);
  }

  public toJS(): ISelectorExpression {
    return {
      matchingType: this.matchingType,
      preparedValue: this.preparedValue,
      returnType: this.returnType,
      type: this.type,
      value: this.value,
    };
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

}
