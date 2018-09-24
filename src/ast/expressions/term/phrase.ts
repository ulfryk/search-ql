import { List, Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Expression, ExpressionType, ValueType } from '../../../common/model';
import { IPhraseExpression } from '../../../dto';
import { InvalidExpression } from '../invalid';
import { TermExpression } from './term';

export class PhraseExpression<T> extends TermExpression<string> {

  public static fromTerm<G, C extends TermExpression<G>>(term: C): PhraseExpression<G> {
    if (term.is(PhraseExpression as any)) {
      return term as any as PhraseExpression<G>;
    }
    return new PhraseExpression(term);
  }

  public readonly returnType: ValueType = ValueType.Phrase;
  public readonly type: ExpressionType.Phrase = ExpressionType.Phrase;

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

  public toList(): List<Expression> {
    return List([this as Expression]).concat(this.term.toList()).toList();
  }

  public checkIntegrity(model: Map<string, ValueType>): Expression {
    return this.getIntegrityErrors(model)
      .foldLeft(this as Expression)(InvalidExpression.fromErrors);
  }

  protected toPhrase() {
    return this;
  }

  protected getIntegrityError(_model: Map<string, ValueType>): Maybe<string> {
    // TODO: Integrity (is there anything to check here ?)
    return None<string>();
  }

  private getIntegrityErrors(model: Map<string, ValueType>): Maybe<string[]> {
    const newTerm = this.term.checkIntegrity(model);

    if (newTerm instanceof InvalidExpression) {
      return Some(newTerm.errors.toArray().map(err => `PhraseExpression term error: ${err}`));
    }

    return this.getIntegrityError(model).map(err => [err]);
  }

}
