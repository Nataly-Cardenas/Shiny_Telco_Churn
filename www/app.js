/* Señal perdida (Shiny) — navegación por fragmentos, idioma, tema, pestañas, deslizadores, tablas y asistente.
   Equivale a los callbacks del navegador (clientside) y a los componentes de Dash del dashboard original. */
(function () {
  'use strict';
  var store = {
    get: function (k, d) { try { var v = JSON.parse(localStorage.getItem(k)); return v == null ? d : v; } catch (e) { return d; } },
    set: function (k, v) { try { localStorage.setItem(k, JSON.stringify(v)); } catch (e) {} }
  };
  var DATA_PAGES = ['eda'];
  var titles = null, suffix = ' · Señal perdida';
  function q(s, r) { return (r || document).querySelector(s); }
  function qa(s, r) { return Array.prototype.slice.call((r || document).querySelectorAll(s)); }
  function send(id, v) { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(id, v, { priority: 'event' }); }

  /* ---------- navegación ---------- */
  function routeKey() { var k = (location.hash || '#inicio').slice(1); return document.getElementById('nav-' + k) || k === 'inicio' ? k : 'inicio'; }
  function applyRoute(scroll) {
    var key = routeKey();
    qa('.nav-link').forEach(function (a) { a.classList.toggle('on', a.id === 'nav-' + key); });
    var f = q('#filters'); if (f) f.style.display = DATA_PAGES.indexOf(key) >= 0 ? '' : 'none';
    var shell = q('#shell'); if (shell) shell.className = key === 'inicio' ? 'shell cover-mode' : 'shell';
    if (titles) document.title = (titles[key] || titles.inicio) + suffix;
    if (scroll) window.scrollTo({ top: 0, behavior: 'smooth' });
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('route', key);
  }
  window.addEventListener('hashchange', function () { applyRoute(true); });

  /* ---------- tema e idioma ---------- */
  function setTheme(t) { document.documentElement.dataset.theme = t; store.set('theme', t); }
  function setLang(l) {
    store.set('lang', l); document.documentElement.lang = l;
    q('#lang-es').classList.toggle('on', l !== 'en'); q('#lang-en').classList.toggle('on', l === 'en');
    send('lang', l);
  }
  document.addEventListener('click', function (e) {
    var t = e.target.closest ? e.target : e.target.parentElement;
    if (t.closest('#theme-btn')) setTheme(document.documentElement.dataset.theme === 'light' ? 'dark' : 'light');
    if (t.closest('#lang-es')) setLang('es');
    if (t.closest('#lang-en')) setLang('en');
    var tab = t.closest('.tabs .tab');
    if (tab) {
      var bar = tab.parentElement;
      qa('.tab', bar).forEach(function (x) { x.classList.toggle('on', x === tab); });
      bar.dataset.value = tab.dataset.i;
      Shiny.setInputValue(bar.id, parseInt(tab.dataset.i, 10));
    }
    if (t.closest('#chat-fab')) q('#chat').classList.toggle('open');
    if (t.closest('#chat-close')) q('#chat').classList.remove('open');
    if (t.closest('#chat-new')) send('chat_new', Date.now());
    var chip = t.closest('.chat-chip'); if (chip) ask(chip.dataset.q);
  });

  /* ---------- asistente ---------- */
  function ask(text) { text = (text || '').trim(); if (!text) return; send('chat_ask', { text: text, nonce: Date.now() }); q('#chat-messages').classList.add('loading'); }
  // El botón de enviar no es type="submit": Shiny retiene todas las entradas si la página tiene uno.
  function sendChat() { var i = q('#chat-input'); ask(i.value); i.value = ''; }
  document.addEventListener('submit', function (e) { if (e.target.id === 'chat-form') { e.preventDefault(); sendChat(); } });
  document.addEventListener('click', function (e) { if (e.target.closest && e.target.closest('#chat-send')) sendChat(); });

  /* ---------- deslizadores (equivalen a dcc.Slider y dcc.RangeSlider) ---------- */
  function sliderValue(el) {
    var ins = qa('.ds-in', el).map(function (i) { return parseFloat(i.value); });
    return ins.length === 2 ? ins.sort(function (a, b) { return a - b; }) : ins[0];
  }
  function paint(el) {
    var min = parseFloat(el.dataset.min), max = parseFloat(el.dataset.max), v = sliderValue(el);
    var lo = Array.isArray(v) ? v[0] : min, hi = Array.isArray(v) ? v[1] : v;
    var fill = q('.ds-fill', el), p = function (x) { return 100 * (x - min) / (max - min); };
    // El control nativo recorre el ancho menos el tamaño de la manija (12 px): marcas y relleno siguen la misma escala.
    var at = function (pc) { return 'calc(' + pc + '% + ' + (6 - 0.12 * pc) + 'px)'; };
    var a0 = Array.isArray(v) ? p(lo) : 0;
    fill.style.left = Array.isArray(v) ? at(a0) : '0'; fill.style.width = 'calc(' + (p(hi) - a0) + '% - ' + (0.12 * (p(hi) - a0)) + 'px' + (Array.isArray(v) ? '' : ' + 6px') + ')';
    var dec = (String(el.dataset.step).split('.')[1] || '').length;
    var boxes = qa('.ds-box', el);
    if (Array.isArray(v)) { boxes[0].value = lo; boxes[1].value = hi; } else { boxes[0].value = +hi.toFixed(dec); }
    boxes.forEach(function (bx) { bx.style.width = Math.max(34, 14 + 9.5 * String(bx.value).length) + 'px'; });
    qa('.ds-marks span', el).forEach(function (m) { var x = parseFloat(m.dataset.p); m.style.left = at(x); m.classList.toggle('in', x >= p(lo) - 0.01 && x <= p(hi) + 0.01); });
    var tip = q('.ds-tip', el); if (tip) { tip.textContent = String(+hi.toFixed(dec)); tip.style.left = at(p(hi)); }
  }
  var sliderBinding = new Shiny.InputBinding();
  jQuery.extend(sliderBinding, {
    find: function (scope) { return jQuery(scope).find('.dslider'); },
    getValue: function (el) { return sliderValue(el); },
    setValue: function (el, v) { var ins = qa('.ds-in', el); if (ins.length === 2) { ins[0].value = v[0]; ins[1].value = v[1]; } else ins[0].value = v; paint(el); },
    subscribe: function (el, cb) {
      paint(el);
      jQuery(el).on('input.dslider', '.ds-in', function () { paint(el); cb(true); });
      jQuery(el).on('change.dslider', '.ds-box', function () {
        var boxes = qa('.ds-box', el), ins = qa('.ds-in', el);
        if (ins.length === 2) { ins[0].value = boxes[0].value; ins[1].value = boxes[1].value; } else ins[0].value = boxes[0].value;
        paint(el); cb(false);
      });
    },
    unsubscribe: function (el) { jQuery(el).off('.dslider'); },
    getRatePolicy: function () { return { policy: 'debounce', delay: 250 }; }
  });
  Shiny.inputBindings.register(sliderBinding, 'senal.slider');

  /* ---------- tablas (equivalen a dash_table.DataTable: orden, filtro y paginación) ---------- */
  function initTable(el) {
    if (el.dataset.ready) return; el.dataset.ready = '1';
    var tbody = q('tbody', el), rows = qa('tr', tbody), size = parseInt(el.dataset.pageSize || '10', 10), page = 0, sortCol = -1, dir = 1;
    var heads = qa('thead tr:first-child th', el), filters = qa('.dt-filter input', el), pager = q('.dt-pager', el), view = rows;
    function cellVal(r, j, num) { var t = r.children[j].textContent; return num ? (t === '' ? -Infinity : parseFloat(t)) : t.toLowerCase(); }
    function render() {
      view = rows.filter(function (r) {
        return filters.every(function (f, j) { var q = f.value.trim().toLowerCase(); return !q || r.children[j].textContent.toLowerCase().indexOf(q) >= 0; });
      });
      if (sortCol >= 0) {
        var num = heads[sortCol].dataset.num === '1';
        view = view.slice().sort(function (a, b) { var x = cellVal(a, sortCol, num), y = cellVal(b, sortCol, num); return (x > y ? 1 : x < y ? -1 : 0) * dir; });
      }
      var pages = Math.max(1, Math.ceil(view.length / size)); page = Math.min(page, pages - 1);
      tbody.innerHTML = ''; view.slice(page * size, page * size + size).forEach(function (r) { tbody.appendChild(r); });
      heads.forEach(function (h, j) { h.dataset.sort = j === sortCol ? (dir > 0 ? 'asc' : 'desc') : ''; });
      pager.innerHTML = pages > 1 ? '<button data-p="first">«</button><button data-p="prev">‹</button><span><input type="number" min="1" max="' + pages + '" value="' + (page + 1) + '"> / ' + pages + '</span><button data-p="next">›</button><button data-p="last">»</button>' : '';
    }
    heads.forEach(function (h, j) { h.addEventListener('click', function () { if (sortCol === j) dir = -dir; else { sortCol = j; dir = 1; } render(); }); });
    filters.forEach(function (f) { f.addEventListener('input', function () { page = 0; render(); }); });
    pager.addEventListener('click', function (e) {
      var b = e.target.closest('button'); if (!b) return; var pages = Math.ceil(view.length / size);
      page = { first: 0, prev: Math.max(0, page - 1), next: Math.min(pages - 1, page + 1), last: pages - 1 }[b.dataset.p]; render();
    });
    pager.addEventListener('change', function (e) { var v = parseInt(e.target.value, 10); if (v > 0) { page = v - 1; render(); } });
    render();
  }

  function scan() { qa('.dtable').forEach(initTable); }
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });

  /* ---------- mensajes del servidor ---------- */
  jQuery(document).on('shiny:connected', function () {
    setTheme(store.get('theme', 'dark'));
    setLang(store.get('lang', 'es'));
    applyRoute(false);
  });
  Shiny.addCustomMessageHandler('chrome', function (m) {
    q('#logo-text').innerHTML = '<b></b> '; q('#logo-text b').textContent = m.logo_b; q('#logo-text').appendChild(document.createTextNode(m.logo));
    Object.keys(m.nav).forEach(function (k) { var s = document.getElementById('nav-t-' + k); if (s) s.textContent = m.nav[k]; });
    Object.keys(m.labels).forEach(function (k) { var l = document.getElementById(k); if (l) l.textContent = m.labels[k]; });
    q('#reset-t').textContent = m.reset; q('#footer-l').textContent = m.footer; q('#theme-btn').title = m.theme;
    titles = m.titles; suffix = m.suffix; document.title = (titles[routeKey()] || titles.inicio) + suffix;
  });
  Shiny.addCustomMessageHandler('chat-texts', function (m) {
    q('#chat-quota').textContent = m.quota; q('#chat-title').textContent = m.title; q('#chat-sub').textContent = m.sub;
    q('#chat-input').placeholder = m.placeholder; q('#chat-new-t').textContent = m.new;
  });
  Shiny.addCustomMessageHandler('slider-set', function (m) {
    var el = document.getElementById(m.id); if (!el) return;
    sliderBinding.setValue(el, m.value); jQuery(el).find('.ds-in').first().trigger('input');
  });
  jQuery(document).on('shiny:value', function (e) {
    if (e.name === 'chat-messages') setTimeout(function () { var m = q('#chat-messages'); m.classList.remove('loading'); m.scrollTop = m.scrollHeight; }, 60);
  });
})();
