

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.authors DISABLE TRIGGER ALL;

INSERT INTO public.authors (id, name, posts_count, tags, created_at, updated_at) VALUES ('b41c627a-a81f-48a5-8ee9-56a4bff702a4', 'rain niké-laos', 0, '{rain,poetry}', '2021-01-01 02:25:57.47583+01', '2021-01-01 02:25:57.47583+01');


ALTER TABLE public.authors ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;

INSERT INTO public.posts (id, title, body) VALUES ('c2eacdc4-1828-40bd-921e-d7a939edc9c3', 'Hello', 'World');
INSERT INTO public.posts (id, title, body) VALUES ('3b07ad25-1474-4fdc-9ab8-6cbb41769876', 'Hello World!', 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam');
INSERT INTO public.posts (id, title, body) VALUES ('9901808b-62d0-4229-b4b5-c6cfdf073d2b', 'Empty Body', '');
INSERT INTO public.posts (id, title, body) VALUES ('ed8f7880-9bbd-41cb-9cf2-c87c675fa8d5', 'Hi', 'Hooooooooo');


ALTER TABLE public.posts ENABLE TRIGGER ALL;


