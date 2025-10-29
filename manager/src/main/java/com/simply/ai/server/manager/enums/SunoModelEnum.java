package com.simply.ai.server.manager.enums;

import com.simply.ai.server.manager.constant.SunoMusicConstant;
import lombok.Getter;

@Getter
public enum SunoModelEnum {
    V3_5("V3_5", "V3.5 - 更好的歌曲结构，最长4分钟"),
    V4("V4", "V4 - 改进的人声质量，最长4分钟"),
    V4_5("V4_5", "V4.5 - 更智能的提示词，更快的生成速度，最长8分钟"),
    V4_5PLUS("V4_5PLUS", "V4.5+ - 音色更丰富，新的创作方式，最长8分钟"),
    V5("V5", "V5 - 更卓越的音乐表现力，生成速度更快，最长8分钟");

    private final String code;
    private final String description;

    SunoModelEnum(String code, String description) {
        this.code = code;
        this.description = description;
    }

    public static SunoModelEnum getByCode(String code) {
        for (SunoModelEnum value : values()) {
            if (value.getCode().equals(code)) {
                return value;
            }
        }
        return null;
    }

    /**
     * 获取模型对应的提示词最大长度
     */
    public int getPromptMaxLength() {
        return this == V3_5 || this == V4 ?
                SunoMusicConstant.PROMPT_MAX_LENGTH_V3_V4 :
                SunoMusicConstant.PROMPT_MAX_LENGTH_V4_5_PLUS_V5;
    }

    /**
     * 获取模型对应的风格最大长度
     */
    public int getStyleMaxLength() {
        return this == V3_5 || this == V4 ?
                SunoMusicConstant.STYLE_MAX_LENGTH_V3_V4 :
                SunoMusicConstant.STYLE_MAX_LENGTH_V4_5_PLUS_V5;
    }

    /**
     * 获取模型对应的标题最大长度
     */
    public int getTitleMaxLength() {
        return this == V3_5 || this == V4 ?
                SunoMusicConstant.TITLE_MAX_LENGTH_V3_V4 :
                SunoMusicConstant.TITLE_MAX_LENGTH_V4_5_PLUS_V5;
    }
}