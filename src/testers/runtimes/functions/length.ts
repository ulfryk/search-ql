import { Maybe } from 'monet';

import { FunctionRuntime, NodeEvaluation } from '../../model';

export const lengthRuntime: FunctionRuntime<number> = (values, ast) => args =>
  NodeEvaluation.ofNumber(values, ast)(
    Maybe.fromNull(args.first())
      .map(getMatch => getMatch().value)
      .flatMap(fieldName => Maybe.fromNull(values.get(fieldName)))
      .fold(0)(value => value.length));
