package com.simply.ai.server.web.common.enums;

/**
 * Veo视频生成模式枚举
 */
public enum VeoGenerationTypeEnum {

    TEXT_2_VIDEO("TEXT_2_VIDEO", "文生视频"),
    FIRST_AND_LAST_FRAMES_2_VIDEO("FIRST_AND_LAST_FRAMES_2_VIDEO", "首尾帧生视频"),
    REFERENCE_2_VIDEO("REFERENCE_2_VIDEO", "参考图生视频");

    private final String code;
    private final String description;

    VeoGenerationTypeEnum(String code, String description) {
        this.code = code;
        this.description = description;
    }

    public String getCode() {
        return code;
    }

    public String getDescription() {
        return description;
    }

    public static VeoGenerationTypeEnum getByCode(String code) {
        for (VeoGenerationTypeEnum type : values()) {
            if (type.getCode().equals(code)) {
                return type;
            }
        }
        return null;
    }
}