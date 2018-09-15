import { ExpressionType } from '../../../common/model';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class TextExpression extends TermExpression<string> {

  public static fromTerm({ value }: TermExpression) {
    return TextExpression.of(value);
  }

  public static of(value: string) {
    return new TextExpression(value);
  }

  public readonly type = ExpressionType.Text;

  constructor(value: string, preparedValue = value.trim()) {
    super(value, preparedValue);
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

}
