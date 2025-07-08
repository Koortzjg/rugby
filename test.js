matchDataSixNations = (await FileAttachment("rugby.csv").csv({ typed: true }))
  .filter(
    d =>
      d.competition_name.includes("Six Nations") &&
      new Date(d.match_date) >= new Date("2000-01-01")
  )
  .map(d => ({
    ...d,
    home_score: +d.home_score,
    away_score: +d.away_score,
    match_date: new Date(d.match_date),
    winner: d.home_score > d.away_score ? d.home_team : d.away_team,
    loser: d.home_score > d.away_score ? d.away_team : d.home_team
  }));

teamStatsSixNations = {
  const stats = new Map();
  for (const d of matchDataSixNations) {
    for (const team of [d.home_team, d.away_team]) {
      if (!stats.has(team)) stats.set(team, { team, games: 0, wins: 0 });
      stats.get(team).games++;
    }
    stats.get(d.winner).wins++;
  }
  return Array.from(stats.values())
    .map(d => ({
      ...d,
      win_rate: d.wins / d.games
    }))
    .sort((a, b) => b.wins - a.wins || b.games - a.games || b.win_rate - a.win_rate);
};

edgesSixNations = {
  const edgeMap = new Map();
  for (const d of matchDataSixNations) {
    const [team1, team2] = [d.home_team, d.away_team].sort();
    const key = `${team1}|${team2}`;
    if (!edgeMap.has(key))
      edgeMap.set(key, {
        team1,
        team2,
        wins_team1: 0,
        wins_team2: 0,
        total: 0
      });
    const edge = edgeMap.get(key);
    edge.total++;
    if (d.winner === team1) edge.wins_team1++;
    else edge.wins_team2++;
  }
  return Array.from(edgeMap.values()).map(
    ({ team1, team2, wins_team1, wins_team2, total }) => {
      const dominant = wins_team1 >= wins_team2 ? team1 : team2;
      const weaker = wins_team1 < wins_team2 ? team1 : team2;
      return {
        source: dominant,
        target: weaker,
        weight: Math.max(wins_team1, wins_team2) / total
      };
    }
  );
};

nodesSixNations = teamStatsSixNations.map(d => ({
  id: d.team,
  group: d.wins,
  wins: d.wins,
  games: d.games,
  win_rate: d.win_rate,
  title: `${d.team}\nWins: ${d.wins}\nGames: ${d.games}\nWin Rate: ${(d.win_rate * 100).toFixed(1)}%`
}));

circularNodesSixNations = {
  const radius = 250;
  const centerX = width / 2;
  const centerY = 300;
  const angleStep = (2 * Math.PI) / nodesSixNations.length;

  return nodesSixNations.map((node, i) => ({
    ...node,
    fx: centerX + radius * Math.cos(i * angleStep),
    fy: centerY + radius * Math.sin(i * angleStep)
  }));
};
