package com.simply.ai.server.manager.constant;

/**
 * Suno音乐生成相关常量
 */
public final class SunoMusicConstant {

    private SunoMusicConstant() {}

    // 模型类型
    public static final String MODEL_V3_5 = "V3_5";
    public static final String MODEL_V4 = "V4";
    public static final String MODEL_V4_5 = "V4_5";
    public static final String MODEL_V4_5PLUS = "V4_5PLUS";
    public static final String MODEL_V5 = "V5";

    // 字符限制
    public static final int PROMPT_MAX_LENGTH_V3_V4 = 3000;
    public static final int PROMPT_MAX_LENGTH_V4_5_PLUS_V5 = 5000;
    public static final int PROMPT_MAX_LENGTH_NON_CUSTOM = 500;
    public static final int STYLE_MAX_LENGTH_V3_V4 = 200;
    public static final int STYLE_MAX_LENGTH_V4_5_PLUS_V5 = 1000;
    public static final int TITLE_MAX_LENGTH_V3_V4 = 80;
    public static final int TITLE_MAX_LENGTH_V4_5_PLUS_V5 = 100;

    // 数值范围
    public static final double MIN_WEIGHT = 0.0;
    public static final double MAX_WEIGHT = 1.0;
    public static final double WEIGHT_STEP = 0.01;

    // 音频时长限制（秒）
    public static final int MAX_UPLOAD_AUDIO_DURATION = 120; // 2分钟

    // 响应码
    public static final int CODE_SUCCESS = 200;
    public static final int CODE_UNAUTHORIZED = 401;
    public static final int CODE_INSUFFICIENT_CREDITS = 402;
    public static final int CODE_NOT_FOUND = 404;
    public static final int CODE_VALIDATION_ERROR = 422;
    public static final int CODE_RATE_LIMIT = 429;
    public static final int CODE_SERVICE_UNAVAILABLE = 455;
    public static final int CODE_SERVER_ERROR = 500;
    public static final int CODE_GENERATION_FAILED = 501;
}