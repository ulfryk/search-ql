import { None, Some } from 'monet';

import { Expression, ExpressionType } from '../../../common/model';
import { InvalidExpression } from '../invalid';
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

  public checkIntegrity(): Expression {
    return this.getIntegrityError()
      .foldLeft(this as Expression)(InvalidExpression.fromError);
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

  private getIntegrityError() {
    return this.value.trim() === this.preparedValue ? None<string>() :
      Some('Values of TextExpression don\'t match: ' +
        `{ value: ${this.value.trim()}, preparedValue: ${this.preparedValue}}`);
  }

}
