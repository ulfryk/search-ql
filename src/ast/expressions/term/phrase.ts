import { Map } from 'immutable';
import { None } from 'monet';

import { Expression, ExpressionType, ValueType } from '../../../common/model';
import { IPhraseExpression } from '../../../dto/expressions';
import { TermExpression } from './term';

export class PhraseExpression<T> extends TermExpression<string> {

  public static fromTerm<G, C extends TermExpression<G>>(term: C): PhraseExpression<G> {
    if (term.is(PhraseExpression as any)) {
      return term as any as PhraseExpression<G>;
    }
    return new PhraseExpression(term);
  }

  public readonly returnType: ValueType = ValueType.Phrase;
  public readonly type = ExpressionType.Phrase;

  constructor(public readonly term: TermExpression<T>) {
    super(term.value, term.value.trim());
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof PhraseExpression &&
      this.preparedValue === other.preparedValue &&
      this.term.equals(other.term)
    );
  }

  public toJS(): IPhraseExpression<T> {
    return {
      preparedValue: this.preparedValue,
      returnType: this.returnType,
      term: this.term.toJS(),
      type: this.type,
      value: this.value,
    };
  }

  protected toPhrase() {
    return this;
  }

  protected getIntegrityError(_model: Map<string, ValueType>) {
    // TODO: Integrity
    return None<string>();
  }

}
