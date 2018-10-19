import { parse, ParsedComponents, ParsedResult } from 'chrono-node';
import { isValid } from 'date-fns';
import { Maybe } from 'monet';

const hasTooFewInfo = (chrono: ParsedResult[]) => chrono.length === 0;

const hasTooMuchInfo = (chrono: ParsedResult[]) => chrono.length > 1;

const hasYear = (components: ParsedComponents) => components.isCertain('year');

const isValidTimeFrame = (chrono: ParsedResult[]) =>
  !hasTooFewInfo(chrono) &&
  !hasTooMuchInfo(chrono) &&
  hasYear(chrono[0].start) &&
  hasYear(Maybe.fromNull(chrono[0].end).orJust(chrono[0].start));

export const isDate = (value: string): boolean =>
  /^[a-z0-9:\-\/,\s\.]+$/ig.test(value) &&
  isValid(new Date(value)) &&
  isValidTimeFrame(parse(value));
