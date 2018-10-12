import { Expression, ExpressionType } from '../../../common/model';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class TextExpression extends TermExpression {

  public static fromTerm({ value }: TermExpression) {
    return TextExpression.of(value);
  }

  public static of(value: string) {
    return new TextExpression(value);
  }

  public readonly type: ExpressionType.Text = ExpressionType.Text;

  constructor(value: string) {
    super(value);
  }

  public checkIntegrity(): Expression {
    return this;
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

}
