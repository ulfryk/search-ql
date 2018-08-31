import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Expression } from '../../ast';
import { Match } from '../../match';

export type FunctionRuntime = (...args: Expression[]) =>
  (values: Map<string, string>, ...matches: Maybe<Map<string, Match>>[]) =>
    Maybe<Map<string, Match>>;
