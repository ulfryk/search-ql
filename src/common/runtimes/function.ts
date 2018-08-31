import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Match } from '../model';

export type FunctionRuntime =
  (values: Map<string, string>, ...matches: Maybe<Map<string, Match>>[]) =>
    Maybe<Map<string, Match>>;
